use crate::lexer::Token;

pub fn take_if<'a, T, F: FnMut(&T) -> bool>(
    mut f: F,
) -> impl FnMut(&'a [T]) -> nom::IResult<&'a [T], &'a T> {
    move |input: &'a [T]| -> nom::IResult<&'a [T], &'a T> {
        if let [first, rest @ ..] = input {
            if f(first) {
                return Ok((rest, first));
            }
        }

        use nom::error::{Error, ErrorKind};
        Err(nom::Err::Error(Error::new(input, ErrorKind::Tag)))
    }
}

pub fn eat_semicolon(input: &[Token]) -> nom::IResult<&[Token], usize> {
    nom::multi::many0_count(take_if(|&t: &Token| t == Token::Semicolon))(input)
}
