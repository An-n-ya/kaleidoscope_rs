use nom::{
    branch::alt,
    bytes::complete::take,
    combinator::{map, opt, verify},
    error::{Error, ErrorKind},
    multi::many0,
    sequence::{pair, tuple},
    Err, IResult,
};

use crate::{
    ast::{Binary, Call, Expr, ExprStmt, Function, Number, Prototype, Stmt, Variable},
    token::{tokenize, Operator, Token, Tokens},
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

impl<'a> Token<'a> {
    pub fn precedence(&self) -> Precedence {
        match self {
            Token::OPERATOR(op) => match op {
                Operator::PLUS | Operator::MINUS => Precedence::Level3,
                Operator::LESS | Operator::GREATER => Precedence::Level2,
            },
            Token::LEFT_PAREN => Precedence::Level7,
            _ => Precedence::Lowest,
        }
    }
}

macro_rules! tag_token {
    ($func_name: ident, $tag: expr) => {
        fn $func_name(tokens: Tokens) -> IResult<Tokens, Tokens> {
            verify(take(1usize), |t: &Tokens| t.tok[0] == $tag)(tokens)
        }
    };
}

tag_token!(def_tag, Token::DEF);
tag_token!(left_paren_tag, Token::LEFT_PAREN);
tag_token!(right_paren_tag, Token::RIGHT_PAREN);
tag_token!(comma_tag, Token::COMMA);
tag_token!(extern_tag, Token::EXTERN);
tag_token!(if_tag, Token::IF);
tag_token!(then_tag, Token::THEN);
tag_token!(else_tag, Token::ELSE);
tag_token!(semicolon_tag, Token::SEMICOLON);
tag_token!(eof_tag, Token::EOF);

fn parse_program(tokens: Tokens) -> IResult<Tokens, Vec<Stmt>> {
    map(pair(many0(statement), eof_tag), |(ret, _)| ret)(tokens)
}

/// top ::= definition | external | expression | ';'
fn statement(tokens: Tokens) -> IResult<Tokens, Stmt> {
    alt((function_definition, extern_stmt, expression_stmt))(tokens)
}

/// primary
///   ::= identifierexpr
///   ::= numberexpr
///   ::= parenexpr
fn primary(input: Tokens) -> IResult<Tokens, Expr> {
    alt((number_expr, identifier_expr, paren_expr))(input)
}

fn number_expr(tokens: Tokens) -> IResult<Tokens, Expr> {
    let (next_tokens, res) = take(1usize)(tokens)?;
    if res.tok.is_empty() {
        Err(Err::Error(Error::new(tokens, ErrorKind::Tag)))
    } else {
        match res.tok[0] {
            Token::NUMBER(v) => Ok((next_tokens, Number::new(v))),
            _ => Err(Err::Error(Error::new(tokens, ErrorKind::Tag))),
        }
    }
}

fn identifier(tokens: Tokens) -> IResult<Tokens, String> {
    let (next_tokens, res) = take(1usize)(tokens)?;
    if res.tok.is_empty() {
        Err(Err::Error(Error::new(tokens, ErrorKind::Tag)))
    } else {
        match res.tok[0] {
            Token::IDENTIFIER(v) => Ok((next_tokens, v.to_string())),
            _ => Err(Err::Error(Error::new(tokens, ErrorKind::Tag))),
        }
    }
}

/// parenexpr ::= '(' expression ')'
fn paren_expr(tokens: Tokens) -> IResult<Tokens, Expr> {
    map(
        tuple((left_paren_tag, expression, right_paren_tag)),
        |(_, expr, _)| expr,
    )(tokens)
}

/// identifierexpr
///   ::= identifier
///   ::= identifier '(' (expression,)* ')'
fn identifier_expr(tokens: Tokens) -> IResult<Tokens, Expr> {
    fn expr_with_comma(tokens: Tokens) -> IResult<Tokens, Expr> {
        map(pair(expression, comma_tag), |(expr, _)| expr)(tokens)
    }

    fn args(tokens: Tokens) -> IResult<Tokens, Vec<Expr>> {
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

fn expression_stmt<'a>(tokens: Tokens) -> IResult<Tokens, Stmt> {
    map(pair(expression, semicolon_tag), |(expr, _)| {
        ExprStmt::new(expr)
    })(tokens)
}
fn expression<'a>(tokens: Tokens) -> IResult<Tokens, Expr> {
    let (tokens, lhs) = primary(tokens)?;
    binary_op_rhs(tokens, lhs, Precedence::Level1)
}

fn binary_op_rhs(
    tokens: Tokens,
    mut lhs: Expr,
    expr_precedence: Precedence,
) -> IResult<Tokens, Expr> {
    let mut next_tokens = tokens;
    loop {
        let (n_next_tokens, ops) = take(1usize)(next_tokens)?;
        let op = ops.tok[0].clone();
        let tok_precedence = op.precedence();
        if tok_precedence < expr_precedence {
            return Ok((next_tokens, lhs));
        }
        next_tokens = n_next_tokens;

        let (n_next_tokens, mut rhs) = primary(next_tokens)?;
        next_tokens = n_next_tokens;
        let (_, ops) = take(1usize)(next_tokens)?;
        let next_precedence = ops.tok[0].clone().precedence();
        if tok_precedence < next_precedence {
            (next_tokens, rhs) = binary_op_rhs(next_tokens, rhs, tok_precedence.next_level())?;
        }

        lhs = Binary::new(op.to_operator(), Box::new(lhs), Box::new(rhs));
    }
}

/// prototype
///   ::= id '(' id* ')'
fn prototype(tokens: Tokens) -> IResult<Tokens, Stmt> {
    fn expr_with_comma(tokens: Tokens) -> IResult<Tokens, String> {
        map(pair(identifier, comma_tag), |(expr, _)| expr)(tokens)
    }

    fn args(tokens: Tokens) -> IResult<Tokens, Vec<String>> {
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
fn function_definition(tokens: Tokens) -> IResult<Tokens, Stmt> {
    map(
        tuple((def_tag, prototype, expression, semicolon_tag)),
        |(_, proto, expr, _)| Function::new(proto, expr),
    )(tokens)
}

/// external ::= 'extern' prototype
fn extern_stmt(tokens: Tokens) -> IResult<Tokens, Stmt> {
    map(
        tuple((extern_tag, prototype, semicolon_tag)),
        |(_, proto, _)| proto,
    )(tokens)
}

pub struct Parser {}

impl Parser {
    pub fn parse(&self, input: &str) -> Vec<Stmt> {
        let (_, mut tokens) = tokenize(input).expect("cannot tokenize");
        tokens.push(Token::EOF);
        let (_, res) = parse_program(Tokens::new(&tokens)).expect("parse failed");
        res
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test1() {
        let parser = Parser {};
        let input = "id < id + 2 < 1;
def foo(arg1, arg2) arg1 + arg2;
extern boo(arg1);";
        let res = parser.parse(input);
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
    }
}
