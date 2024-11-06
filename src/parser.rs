use miette::bail;
use nom::{
    branch::alt,
    bytes::complete::take,
    combinator::{map, opt, verify},
    error::{Error, ErrorKind},
    multi::many0,
    sequence::{pair, tuple},
    Err, Finish, IResult, Offset,
};

use crate::{
    ast::{Binary, Call, Expr, ExprStmt, Function, Number, Prototype, Stmt, Variable},
    token::{tokenize, Operator, Token, TokenKind, Tokens},
};

// refer to monkey-rust

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    Lowest,
    Level1,
    Level2,
    Level3,
    Level4,
    Level5,
    Level6,
    Level7,
    Highest,
}

impl Precedence {
    pub fn next_level(self) -> Precedence {
        use Precedence::*;
        // FIXME: is it right? need testcase
        match self {
            Lowest => Level1,
            Level1 => Level2,
            Level2 => Level3,
            Level3 => Level4,
            Level4 => Level5,
            Level5 => Level6,
            Level6 => Level7,
            Level7 => Highest,
            Highest => Highest,
        }
    }
}

impl<'a> TokenKind<'a> {
    pub fn precedence(&self) -> Precedence {
        match self {
            TokenKind::OPERATOR(op) => match op {
                Operator::PLUS | Operator::MINUS => Precedence::Level3,
                Operator::LESS | Operator::GREATER => Precedence::Level2,
            },
            TokenKind::LEFT_PAREN => Precedence::Level7,
            _ => Precedence::Lowest,
        }
    }
}

pub struct ParserError<'a> {
    input: Tokens<'a>,
    kind: ParserErrorKind,
}
pub enum ParserErrorKind {
    Default,
    Expect(String),
}
impl<'a> ParserError<'a> {
    pub fn new(input: Tokens<'a>, kind: ParserErrorKind) -> Self {
        Self { input, kind }
    }
}
impl<'a> nom::error::ParseError<Tokens<'a>> for ParserError<'a> {
    fn from_error_kind(input: Tokens<'a>, _kind: ErrorKind) -> Self {
        ParserError {
            input: input.clone(),
            kind: ParserErrorKind::Default,
        }
    }

    fn append(input: Tokens<'a>, kind: ErrorKind, _other: Self) -> Self {
        Self::from_error_kind(input, kind)
    }
}

macro_rules! tag_token {
    ($func_name: ident, $tag: expr) => {
        fn $func_name(tokens: Tokens) -> IResult<Tokens, Tokens, ParserError> {
            verify(take(1usize), |t: &Tokens| t.tok[0].kind == $tag)(tokens)
                .finish()
                .map_err(|_e: ParserError| {
                    Err::Error(ParserError::new(
                        tokens,
                        ParserErrorKind::Expect(format!("{:?}", $tag)),
                    ))
                })
        }
    };
}

tag_token!(def_tag, TokenKind::DEF);
tag_token!(left_paren_tag, TokenKind::LEFT_PAREN);
tag_token!(right_paren_tag, TokenKind::RIGHT_PAREN);
tag_token!(comma_tag, TokenKind::COMMA);
tag_token!(extern_tag, TokenKind::EXTERN);
tag_token!(if_tag, TokenKind::IF);
tag_token!(then_tag, TokenKind::THEN);
tag_token!(else_tag, TokenKind::ELSE);
tag_token!(semicolon_tag, TokenKind::SEMICOLON);
tag_token!(eof_tag, TokenKind::EOF);

fn parse_program(tokens: Tokens) -> IResult<Tokens, Vec<Stmt>, ParserError> {
    map(pair(many0(statement), eof_tag), |(ret, _)| ret)(tokens)
}

/// top ::= definition | external | expression | ';'
fn statement(tokens: Tokens) -> IResult<Tokens, Stmt, ParserError> {
    alt((function_definition, extern_stmt, expression_stmt))(tokens)
}

/// primary
///   ::= identifierexpr
///   ::= numberexpr
///   ::= parenexpr
fn primary(input: Tokens) -> IResult<Tokens, Expr, ParserError> {
    alt((number_expr, identifier_expr, paren_expr))(input)
}

fn number_expr(tokens: Tokens) -> IResult<Tokens, Expr, ParserError> {
    let (next_tokens, res) = take(1usize)(tokens)?;
    if res.tok.is_empty() {
        Err(Err::Error(ParserError::new(
            tokens,
            ParserErrorKind::Default,
        )))
    } else {
        match res.tok[0].kind {
            TokenKind::NUMBER(v) => Ok((next_tokens, Number::new(v))),
            _ => Err(Err::Error(ParserError::new(
                tokens,
                ParserErrorKind::Expect("Number".to_string()),
            ))),
        }
    }
}

fn identifier(tokens: Tokens) -> IResult<Tokens, String, ParserError> {
    let (next_tokens, res) = take(1usize)(tokens)?;
    if res.tok.is_empty() {
        Err(Err::Error(ParserError::new(
            tokens,
            ParserErrorKind::Default,
        )))
    } else {
        match res.tok[0].kind {
            TokenKind::IDENTIFIER(v) => Ok((next_tokens, v.to_string())),
            _ => Err(Err::Error(ParserError::new(
                tokens,
                ParserErrorKind::Expect("Number".to_string()),
            ))),
        }
    }
}

/// parenexpr ::= '(' expression ')'
fn paren_expr(tokens: Tokens) -> IResult<Tokens, Expr, ParserError> {
    map(
        tuple((left_paren_tag, expression, right_paren_tag)),
        |(_, expr, _)| expr,
    )(tokens)
}

/// identifierexpr
///   ::= identifier
///   ::= identifier '(' (expression,)* ')'
fn identifier_expr(tokens: Tokens) -> IResult<Tokens, Expr, ParserError> {
    fn expr_with_comma(tokens: Tokens) -> IResult<Tokens, Expr, ParserError> {
        map(pair(expression, comma_tag), |(expr, _)| expr)(tokens)
    }

    fn args(tokens: Tokens) -> IResult<Tokens, Vec<Expr>, ParserError> {
        map(
            tuple((many0(expr_with_comma), expression, opt(comma_tag))),
            |(mut exprs, expr, _)| {
                exprs.push(expr);
                exprs
            },
        )(tokens)
    }

    alt((
        map(
            tuple((identifier, left_paren_tag, args, right_paren_tag)),
            |(callee, _, args, _)| Call::new(callee, args),
        ),
        map(identifier, |v| Variable::new(v)),
    ))(tokens)
}

fn expression_stmt<'a>(tokens: Tokens) -> IResult<Tokens, Stmt, ParserError> {
    map(pair(expression, semicolon_tag), |(expr, _)| {
        ExprStmt::new(expr)
    })(tokens)
}
fn expression<'a>(tokens: Tokens) -> IResult<Tokens, Expr, ParserError> {
    let (tokens, lhs) = primary(tokens)?;
    binary_op_rhs(tokens, lhs, Precedence::Level1)
}

fn binary_op_rhs(
    tokens: Tokens,
    mut lhs: Expr,
    expr_precedence: Precedence,
) -> IResult<Tokens, Expr, ParserError> {
    let mut next_tokens = tokens;
    loop {
        let (n_next_tokens, ops) = take(1usize)(next_tokens)?;
        let op = ops.tok[0].clone();
        let tok_precedence = op.kind.precedence();
        if tok_precedence < expr_precedence {
            return Ok((next_tokens, lhs));
        }
        next_tokens = n_next_tokens;

        let (n_next_tokens, mut rhs) = primary(next_tokens)?;
        next_tokens = n_next_tokens;
        let (_, ops) = take(1usize)(next_tokens)?;
        let next_precedence = ops.tok[0].clone().kind.precedence();
        if tok_precedence < next_precedence {
            (next_tokens, rhs) = binary_op_rhs(next_tokens, rhs, tok_precedence.next_level())?;
        }

        lhs = Binary::new(op.to_operator(), Box::new(lhs), Box::new(rhs));
    }
}

/// prototype
///   ::= id '(' id* ')'
fn prototype(tokens: Tokens) -> IResult<Tokens, Stmt, ParserError> {
    fn expr_with_comma(tokens: Tokens) -> IResult<Tokens, String, ParserError> {
        map(pair(identifier, comma_tag), |(expr, _)| expr)(tokens)
    }

    fn args(tokens: Tokens) -> IResult<Tokens, Vec<String>, ParserError> {
        map(
            tuple((many0(expr_with_comma), identifier, opt(comma_tag))),
            |(mut exprs, expr, _)| {
                exprs.push(expr);
                exprs
            },
        )(tokens)
    }

    map(
        tuple((identifier, left_paren_tag, args, right_paren_tag)),
        |(callee, _, args, _)| Prototype::new(callee, args),
    )(tokens)
}

/// definition ::= 'def' prototype expression
fn function_definition(tokens: Tokens) -> IResult<Tokens, Stmt, ParserError> {
    let (tokens, _) = def_tag(tokens)?;
    let (tokens, proto) = prototype(tokens).finish().map_err(|_e| {
        Err::Failure(ParserError::new(
            tokens,
            ParserErrorKind::Expect("PROTOTYPE".to_string()),
        ))
    })?;
    let (tokens, expr) = expression(tokens).finish().map_err(|e| {
        Err::Failure(ParserError::new(
            tokens,
            ParserErrorKind::Expect("EXPRESSION".to_string()),
        ))
    })?;
    let (tokens, _) = semicolon_tag(tokens)
        .finish()
        .map_err(|e| Err::Failure(e))?;
    Ok((tokens, Function::new(proto, expr)))
}

/// external ::= 'extern' prototype
fn extern_stmt(tokens: Tokens) -> IResult<Tokens, Stmt, ParserError> {
    let (tokens, _) = extern_tag(tokens)?;
    let (tokens, proto) = prototype(tokens).finish().map_err(|_e| {
        Err::Failure(ParserError::new(
            tokens,
            ParserErrorKind::Expect("PROTOTYPE".to_string()),
        ))
    })?;
    let (tokens, _) = semicolon_tag(tokens)
        .finish()
        .map_err(|e| Err::Failure(e))?;
    Ok((tokens, proto))
}

pub struct Parser {}

impl Parser {
    pub fn parse(&self, input: &str) -> Result<Vec<Stmt>, miette::Error> {
        let (remain, mut tokens) = tokenize(input).expect("cannot tokenize");
        tokens.push(Token {
            kind: TokenKind::EOF,
            origin: remain,
            code: input,
        });
        let (_, res) = parse_program(Tokens::new(&tokens))
            .finish()
            .map_err(|e| {
                let toks = e.input.tok;
                if toks.is_empty() {
                    miette::bail!("empty input!")
                } else {
                    let tok = toks[0].clone();
                    let offset = input.offset(tok.code);
                    let help = match e.kind {
                        ParserErrorKind::Default => "".to_string(),
                        ParserErrorKind::Expect(expect) => format!("Expecting {expect}"),
                    };
                    // FIXME: what if we have multiple parse error
                    let err: Result<Vec<Stmt>, miette::Error> = Err(miette::miette! {
                        labels = vec![
                            miette::LabeledSpan::at(offset..offset + tok.origin.len(), "here"),
                        ],
                        help = help,
                        "Parse Error"
                    }
                    .with_source_code(input.to_string()));
                    err
                }
            })
            .expect("hello");
        Ok(res)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test1() -> miette::Result<()> {
        let parser = Parser {};
        let input = "id < id + 2 < 1;
def foo(arg1, arg2) arg1 + arg2;
extern boo(arg1);";
        let res = parser.parse(input)?;
        let dumped_stmts = res.iter().map(|stmt| stmt.dump()).collect::<Vec<_>>();
        for s in &dumped_stmts {
            println!("{s}");
        }
        let expect = vec![
            "ExprStmt
  Binary(LESS)
    Binary(LESS)
      Variable(id)
      Binary(PLUS)
        Variable(id)
        Number(2)
    Number(1)
",
            "Function:
prototype:
  Prototype: foo(arg1,arg2)
expr:
  Binary(PLUS)
    Variable(arg1)
    Variable(arg2)
",
            "Prototype: boo(arg1)
",
        ];
        assert_eq!(dumped_stmts, expect);
        Ok(())
    }

    // #[test]
    // fn test_parse_def() -> miette::Result<()> {
    //     let parser = Parser {};
    //     let input = "def 1foo(a1, arg2) arg1 + arg2;";
    //     let _res = parser.parse(input)?;
    //     Ok(())
    // }
}
