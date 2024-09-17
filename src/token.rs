use nom::{
    branch::alt,
    bytes::complete::{is_not, tag},
    character::complete::{alpha1, alphanumeric1, char, line_ending, multispace1},
    combinator::{recognize, value},
    multi::many0,
    number::complete::double,
    sequence::{delimited, pair},
    IResult,
};

#[derive(PartialEq, Debug)]
pub enum Token<'a> {
    EOF,
    DEF,
    EXTERN,
    IDENTIFIER(&'a str),
    NUMBER(f64),
    LEFT_PAREN,
    RIGHT_PAREN,
    PLUS,
    MINUS,
    LESS,
    GREATER,
    IF,
    THEN,
    ELSE,
}

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
    let (input, res) = recognize(pair(alpha1, many0(alphanumeric1)))(input)?;
    Ok((input, Token::IDENTIFIER(res)))
}
fn number(input: &str) -> IResult<&str, Token> {
    let (input, res) = double(input)?;
    Ok((input, Token::NUMBER(res)))
}
fn symbol(input: &str) -> IResult<&str, Token> {
    let (input, res) = alt((tag("<"), tag(">"), tag("+"), tag("-"), tag("("), tag(")")))(input)?;
    Ok((input, {
        match res {
            "<" => Token::LESS,
            ">" => Token::GREATER,
            "+" => Token::PLUS,
            "-" => Token::MINUS,
            "(" => Token::LEFT_PAREN,
            ")" => Token::RIGHT_PAREN,
            _ => unreachable!(),
        }
    }))
}
fn keywords(input: &str) -> IResult<&str, Token> {
    let (input, res) = alt((
        tag("def"),
        tag("extern"),
        tag("if"),
        tag("then"),
        tag("else"),
    ))(input)?;
    Ok((input, {
        match res {
            "def" => Token::DEF,
            "extern" => Token::EXTERN,
            "if" => Token::IF,
            "then" => Token::THEN,
            "else" => Token::ELSE,
            _ => unreachable!(),
        }
    }))
}

fn comment(input: &str) -> IResult<&str, &str> {
    recognize(pair(char('#'), is_not("\n\r")))(input)
}

#[cfg(test)]
mod tests {
    use core::str;

    use super::*;
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
        let res = format!("{:?}", res);
        let expected = br#"[DEF, IDENTIFIER("fib"), LEFT_PAREN, IDENTIFIER("x"), RIGHT_PAREN, IF, IDENTIFIER("x"), LESS, NUMBER(3.0), THEN, NUMBER(1.0), ELSE, IDENTIFIER("fib"), LEFT_PAREN, IDENTIFIER("x"), MINUS, NUMBER(1.0), RIGHT_PAREN, PLUS, IDENTIFIER("fib"), LEFT_PAREN, IDENTIFIER("x"), MINUS, NUMBER(2.0), RIGHT_PAREN, IDENTIFIER("fib"), LEFT_PAREN, NUMBER(40.0), RIGHT_PAREN]"#;
        let expected = str::from_utf8(expected).unwrap();
        assert_eq!(res, expected);
    }
}
