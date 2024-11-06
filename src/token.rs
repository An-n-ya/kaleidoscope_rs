use std::{
    iter::Enumerate,
    ops::{Range, RangeFrom, RangeFull, RangeTo},
};

use miette::LabeledSpan;
use nom::{
    branch::alt,
    bytes::complete::{is_not, tag},
    character::complete::{alpha1, alphanumeric1, char, line_ending, multispace1},
    combinator::{recognize, value},
    multi::many0,
    number::complete::double,
    sequence::{delimited, pair},
    Err::Error,
    Finish, IResult, InputIter, InputLength, InputTake, Needed, Offset, Slice,
};
#[derive(PartialEq, Debug, Clone)]
pub struct Token<'a> {
    pub kind: TokenKind<'a>,
    pub origin: &'a str,
    pub code: &'a str,
}

#[derive(PartialEq, Debug, Clone)]
pub enum TokenKind<'a> {
    EOF,
    DEF,
    EXTERN,
    IDENTIFIER(&'a str),
    NUMBER(f64),
    LEFT_PAREN,
    RIGHT_PAREN,
    COMMA,
    SEMICOLON,
    OPERATOR(Operator),
    IF,
    THEN,
    ELSE,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Operator {
    PLUS,
    MINUS,
    LESS,
    GREATER,
}

impl<'a> Token<'a> {
    pub fn to_operator(&self) -> Operator {
        match &self.kind {
            TokenKind::OPERATOR(op) => op.clone(),
            _ => unreachable!("token {:?} tries to convert to operator", self),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub struct Tokens<'a> {
    pub tok: &'a [Token<'a>],
    pub start: usize,
    pub end: usize,
}

impl<'a> Tokens<'a> {
    pub fn new(vec: &'a [Token]) -> Self {
        Tokens {
            tok: vec,
            start: 0,
            end: vec.len(),
        }
    }
}

impl<'a> InputLength for Tokens<'a> {
    #[inline]
    fn input_len(&self) -> usize {
        self.tok.len()
    }
}

impl<'a> InputTake for Tokens<'a> {
    #[inline]
    fn take(&self, count: usize) -> Self {
        Tokens {
            tok: &self.tok[0..count],
            start: 0,
            end: count,
        }
    }

    #[inline]
    fn take_split(&self, count: usize) -> (Self, Self) {
        let (prefix, suffix) = self.tok.split_at(count);
        let first = Tokens {
            tok: prefix,
            start: 0,
            end: prefix.len(),
        };
        let second = Tokens {
            tok: suffix,
            start: 0,
            end: suffix.len(),
        };
        (second, first)
    }
}

impl<'a> InputLength for Token<'a> {
    #[inline]
    fn input_len(&self) -> usize {
        1
    }
}

impl<'a> Slice<Range<usize>> for Tokens<'a> {
    #[inline]
    fn slice(&self, range: Range<usize>) -> Self {
        Tokens {
            tok: self.tok.slice(range.clone()),
            start: self.start + range.start,
            end: self.start + range.end,
        }
    }
}

impl<'a> Slice<RangeTo<usize>> for Tokens<'a> {
    #[inline]
    fn slice(&self, range: RangeTo<usize>) -> Self {
        self.slice(0..range.end)
    }
}

impl<'a> Slice<RangeFrom<usize>> for Tokens<'a> {
    #[inline]
    fn slice(&self, range: RangeFrom<usize>) -> Self {
        self.slice(range.start..self.end - self.start)
    }
}

impl<'a> Slice<RangeFull> for Tokens<'a> {
    #[inline]
    fn slice(&self, _: RangeFull) -> Self {
        Tokens {
            tok: self.tok,
            start: self.start,
            end: self.end,
        }
    }
}

impl<'a> InputIter for Tokens<'a> {
    type Item = &'a Token<'a>;
    type Iter = Enumerate<::std::slice::Iter<'a, Token<'a>>>;
    type IterElem = ::std::slice::Iter<'a, Token<'a>>;

    #[inline]
    fn iter_indices(&self) -> Enumerate<::std::slice::Iter<'a, Token<'a>>> {
        self.tok.iter().enumerate()
    }
    #[inline]
    fn iter_elements(&self) -> ::std::slice::Iter<'a, Token<'a>> {
        self.tok.iter()
    }
    #[inline]
    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.tok.iter().position(predicate)
    }
    #[inline]
    fn slice_index(&self, count: usize) -> Result<usize, Needed> {
        if self.tok.len() >= count {
            Ok(count)
        } else {
            Err(Needed::Unknown)
        }
    }
}

// TODO: write a Tokenizer to maintain the current pos/line information
// TODO: use miette to report the wrong tokenized symbol
//

pub fn tokenize(input: &str) -> IResult<&str, Vec<Token>> {
    many0(get_token)(input)
}

fn get_token(input: &str) -> IResult<&str, Token> {
    delimited(
        ignorance,
        alt((keywords, symbol, identifier, number)),
        ignorance,
    )(input)
}

fn ignorance(input: &str) -> IResult<&str, ()> {
    value((), many0(alt((comment, line_ending, multispace1))))(input)
}

fn identifier(input: &str) -> IResult<&str, Token> {
    let (remain, res) = recognize(pair(alpha1, many0(alphanumeric1)))(input)?;
    let tok = Token {
        kind: TokenKind::IDENTIFIER(res),
        origin: res,
        code: input,
    };
    Ok((remain, tok))
}
fn number(input: &str) -> IResult<&str, Token> {
    let (remain, res) = double(input)?;
    let offset = input.offset(remain);
    let origin = &input[0..offset];
    let tok = Token {
        kind: TokenKind::NUMBER(res),
        origin,
        code: input,
    };
    Ok((remain, tok))
}
fn symbol(input: &str) -> IResult<&str, Token> {
    let (remain, res) = alt((
        tag("<"),
        tag(">"),
        tag("+"),
        tag("-"),
        tag("("),
        tag(")"),
        tag(","),
        tag(";"),
    ))(input)?;
    let kind = match res {
        "<" => TokenKind::OPERATOR(Operator::LESS),
        ">" => TokenKind::OPERATOR(Operator::GREATER),
        "+" => TokenKind::OPERATOR(Operator::PLUS),
        "-" => TokenKind::OPERATOR(Operator::MINUS),
        "(" => TokenKind::LEFT_PAREN,
        ")" => TokenKind::RIGHT_PAREN,
        "," => TokenKind::COMMA,
        ";" => TokenKind::SEMICOLON,
        _ => unreachable!(),
    };
    let tok = Token {
        kind,
        origin: res,
        code: input,
    };
    Ok((remain, tok))
}
fn keywords(input: &str) -> IResult<&str, Token> {
    let (remain, res) = alt((
        tag("def"),
        tag("extern"),
        tag("if"),
        tag("then"),
        tag("else"),
    ))(input)?;
    let kind = match res {
        "def" => TokenKind::DEF,
        "extern" => TokenKind::EXTERN,
        "if" => TokenKind::IF,
        "then" => TokenKind::THEN,
        "else" => TokenKind::ELSE,
        _ => unreachable!(),
    };
    let tok = Token {
        kind,
        origin: res,
        code: input,
    };
    Ok((remain, tok))
}

fn comment(input: &str) -> IResult<&str, &str> {
    recognize(pair(char('#'), is_not("\n\r")))(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use core::str;

    fn get_kind(tokens: &[Token]) -> String {
        let mut res = vec![];
        for t in tokens {
            res.push(t.kind.clone());
        }
        format!("{:?}", res)
    }

    #[test]
    fn test1() {
        let input = "# Compute the x'th fibonacci number.
def fib(x)
  if x < 3 then
    1
  else
    fib(x-1)+fib(x-2)

# This expression will compute the 40th number.
fib(40)";
        let (_, res) = tokenize(input).expect("cannot parse");
        let res = get_kind(&res);
        let expected = br#"[DEF, IDENTIFIER("fib"), LEFT_PAREN, IDENTIFIER("x"), RIGHT_PAREN, IF, IDENTIFIER("x"), OPERATOR(LESS), NUMBER(3.0), THEN, NUMBER(1.0), ELSE, IDENTIFIER("fib"), LEFT_PAREN, IDENTIFIER("x"), OPERATOR(MINUS), NUMBER(1.0), RIGHT_PAREN, OPERATOR(PLUS), IDENTIFIER("fib"), LEFT_PAREN, IDENTIFIER("x"), OPERATOR(MINUS), NUMBER(2.0), RIGHT_PAREN, IDENTIFIER("fib"), LEFT_PAREN, NUMBER(40.0), RIGHT_PAREN]"#;
        let expected = str::from_utf8(expected).unwrap();
        assert_eq!(res, expected);
    }

    #[test]
    fn semicolon() {
        let input = "id;";
        let (_, res) = tokenize(input).expect("cannot parse");
        let res = get_kind(&res);
        let expected = br#"[IDENTIFIER("id"), SEMICOLON]"#;
        let expected = str::from_utf8(expected).unwrap();
        assert_eq!(res, expected);
    }
}
