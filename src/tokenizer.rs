use nom::{
    IResult, Parser,
    branch::alt,
    bytes::complete::{tag, take_till, take_until},
    character::complete::{alpha1, alphanumeric0, alphanumeric1, anychar, char, digit1, satisfy},
    combinator::{map, map_res, opt, recognize},
    multi::{many_till, many0, many1},
    sequence::{delimited, pair},
};

pub enum Literal {
    Plus,
    Minus,
    Mult,
    Div,
    Mod,
    Question,
    Colon,
    Assign,
    Bang,
    Lcurly,
    Rcurly,
    Semicolon,
    At,
    And,
    Or,
    Lt,
    Gt,
    Gte,
    Lte,
    Eq,
    Neq,
}

pub enum Keyword {
    True,
    False,
    Empty,
}

pub enum Token {
    Literal(Literal),
    Keyword(Keyword),
    Str(String),
    Ident(String),
    LComment(String),
    TComment(String),
    Number(String),
}

pub fn tokenize(input: &str) -> IResult<&str, Vec<Token>> {
    many1(alt((
        literal,
        string,
        lead_comment,
        trail_comment,
        identifier,
        keyword,
        number,
    )))
    .parse(input)
}

fn literal(input: &str) -> IResult<&str, Token> {
    map(
        alt([
            tag("+"),
            tag("-"),
            tag("*"),
            tag("/"),
            tag("%"),
            tag("?"),
            tag(":"),
            tag("="),
            tag("!"),
            tag("{"),
            tag("}"),
            tag("<"),
            tag(">"),
            tag(";"),
            tag("@"),
            tag("&&"),
            tag("||"),
            tag("<="),
            tag(">="),
            tag("=="),
            tag("!="),
        ]),
        |x: &str| match x {
            "+" => Token::Literal(Literal::Plus),
            "-" => Token::Literal(Literal::Minus),
            "*" => Token::Literal(Literal::Mult),
            "/" => Token::Literal(Literal::Div),
            "%" => Token::Literal(Literal::Mod),
            "?" => Token::Literal(Literal::Question),
            ":" => Token::Literal(Literal::Colon),
            "=" => Token::Literal(Literal::Assign),
            "!" => Token::Literal(Literal::Bang),
            "{" => Token::Literal(Literal::Lcurly),
            "}" => Token::Literal(Literal::Rcurly),
            "<" => Token::Literal(Literal::Lt),
            ">" => Token::Literal(Literal::Gt),
            ";" => Token::Literal(Literal::Semicolon),
            "@" => Token::Literal(Literal::At),
            "&&" => Token::Literal(Literal::And),
            "||" => Token::Literal(Literal::Or),
            "<=" => Token::Literal(Literal::Lte),
            ">=" => Token::Literal(Literal::Gte),
            "==" => Token::Literal(Literal::Eq),
            "!=" => Token::Literal(Literal::Neq),
            _ => panic!(),
        },
    )
    .parse(input)
}

fn string(input: &str) -> IResult<&str, Token> {
    let single = delimited(tag("\'"), alphanumeric0, tag("\'"));
    let double = delimited(tag("\""), alphanumeric0, tag("\""));
    let quoted_string = alt((single, double));
    map(recognize(many0(quoted_string)), |x: &str| {
        Token::Str(x.to_owned())
    })
    .parse(input)
}
fn lead_comment(input: &str) -> IResult<&str, Token> {
    map(
        delimited(tag("/*"), take_until("*/"), tag("*/")),
        |x: &str| Token::LComment(x.to_owned()),
    )
    .parse(input)
}

fn trail_comment(input: &str) -> IResult<&str, Token> {
    map(
        delimited(tag("//"), take_until("\n"), tag("\n")),
        |x: &str| Token::TComment(x.to_owned()),
    )
    .parse(input)
}

fn identifier(input: &str) -> IResult<&str, Token> {
    let initial = alt((alpha1, tag("_")));
    let rest = many0(alt((alphanumeric1, tag("_"))));
    map(recognize(pair(initial, rest)), |x: &str| {
        Token::Ident(x.to_owned())
    })
    .parse(input)
}

fn keyword(input: &str) -> IResult<&str, Token> {
    map(
        alt([tag("empty"), tag("true"), tag("false")]),
        |x: &str| match x {
            "empty" => Token::Keyword(Keyword::Empty),
            "true" => Token::Keyword(Keyword::True),
            "false" => Token::Keyword(Keyword::False),
            _ => panic!(),
        },
    )
    .parse(input)
}

fn number(input: &str) -> IResult<&str, Token> {
    map(
        recognize((
            digit1,
            opt((char('e'), opt(alt((char('+'), char('-')))))),
            digit1,
        )),
        |x: &str| Token::Number(x.to_owned()),
    )
    .parse(input)
}
