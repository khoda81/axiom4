use crate::lexer::Token;
use nom::multi::many0_count;
use nom::IResult;

pub fn take_if<'a, T, F: FnMut(&T) -> bool>(
    mut f: F,
) -> impl FnMut(&'a [T]) -> IResult<&'a [T], &'a T> {
    move |input: &'a [T]| -> IResult<&'a [T], &'a T> {
        if let [first, rest @ ..] = input {
            if f(first) {
                return Ok((rest, first));
            }
        }

        Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        )))
    }
}

pub fn eat_newline(input: &[Token]) -> IResult<&[Token], usize> {
    many0_count(take_if(|&t: &Token| t == Token::NewLine))(input)
}
