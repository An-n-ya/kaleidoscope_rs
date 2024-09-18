use nom::{
    bytes::complete::take,
    error::{Error, ErrorKind},
    Err, IResult,
};

use crate::{
    ast::{BinaryExprAST, Expr, ExprAST, NumberExprAST},
    token::{Operator, Token, Tokens},
};

// TODO: add precedence
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

fn primary<'a>(input: Tokens) -> IResult<Tokens, Expr> {
    todo!()
}

fn number_expr<'a>(tokens: Tokens) -> IResult<Tokens, Expr> {
    let (next_tokens, res) = take(1usize)(tokens)?;
    if res.tok.is_empty() {
        Err(Err::Error(Error::new(tokens, ErrorKind::Tag)))
    } else {
        match res.tok[0] {
            Token::NUMBER(v) => Ok((next_tokens, NumberExprAST::new(v))),
            _ => Err(Err::Error(Error::new(tokens, ErrorKind::Tag))),
        }
    }
}

fn expression<'a>(input: Tokens) -> IResult<Tokens, Expr> {
    let (tokens, lhs) = primary(input)?;
    binary_op_rhs(input, lhs, Precedence::Lowest)
}

fn binary_op_rhs(
    tokens: Tokens,
    mut lhs: Expr,
    expr_precedence: Precedence,
) -> IResult<Tokens, Expr> {
    let mut next_tokens = tokens;
    loop {
        let (n_next_tokens, ops) = take(1usize)(next_tokens)?;
        next_tokens = n_next_tokens;
        let op = ops.tok[0].clone();
        let tok_precedence = op.precedence();
        if tok_precedence < expr_precedence {
            return Ok((next_tokens, lhs));
        }

        let (n_next_tokens, mut rhs) = primary(next_tokens)?;
        next_tokens = n_next_tokens;
        let (_, ops) = take(1usize)(next_tokens)?;
        let next_precedence = ops.tok[0].clone().precedence();
        if tok_precedence < next_precedence {
            (next_tokens, rhs) = binary_op_rhs(next_tokens, rhs, tok_precedence.next_level())?;
        }

        lhs = BinaryExprAST::new(op.to_operator(), lhs, rhs);
    }
}
