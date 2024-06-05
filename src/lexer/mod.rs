use std::str::Chars;

pub mod interner;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Token {
    Symbol(interner::Symbol),
    Other(char),
    Comma,
    Colon,
    NewLine,
    SemiColon,

    POpen,
    PClose,

    Bar,
    Bang,

    Plus,
    Minus,
    Star,
    Slash,

    Eq,
    NEq,
    Lt,
    LEq,
    Gt,
    GEq,
}

#[derive(Clone, Debug)]
pub struct Lexer<'a> {
    pub interner: interner::StringInterner,
    cursor: Chars<'a>,
}

impl<'a> Lexer<'a> {
    pub fn new(text: &'a str) -> Self {
        Self::new_with_interner(text, interner::StringInterner::new())
    }

    fn new_with_interner(text: &'a str, interner: interner::StringInterner) -> Self {
        Self {
            interner,
            cursor: text.chars(),
        }
    }

    fn handle_single_char_token(next_char: char) -> Option<Token> {
        let token = match next_char {
            ':' => Token::Colon,
            ';' => Token::SemiColon,
            ',' => Token::Comma,
            '(' => Token::POpen,
            ')' => Token::PClose,
            '|' => Token::Bar,
            '\n' => Token::NewLine,
            '=' => Token::Eq,

            '+' => Token::Plus,
            '-' => Token::Minus,
            '*' => Token::Star,
            '/' => Token::Slash,

            _ => return None,
        };

        Some(token)
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        // Skip whitespace
        let text = self
            .cursor
            .as_str()
            .trim_start_matches(|c: char| c.is_whitespace() && c != '\n');

        self.cursor = text.chars();

        let next_char = self.cursor.next()?;
        if let Some(token) = Self::handle_single_char_token(next_char) {
            return Some(token);
        }

        let is_ident = |c: char| c.is_alphanumeric() || c == '_';

        if is_ident(next_char) {
            let next_token = text.find(|c| !is_ident(c)).unwrap_or(text.len());

            let (ident, rest) = text.split_at(next_token);
            self.cursor = rest.chars();

            // Intern symbol
            let symbol = self.interner.intern(ident);
            return Some(Token::Symbol(symbol));
        }

        let before_eq = self.cursor.clone().next() == Some('=');
        if before_eq {
            // Move cursor forward
            self.cursor.next();
        }

        let token = match next_char {
            '!' if before_eq => Token::NEq,
            '!' => Token::Bang,

            '<' if before_eq => Token::LEq,
            '<' => Token::Lt,

            '>' if before_eq => Token::GEq,
            '>' => Token::Gt,

            c => {
                self.cursor = text.chars();
                self.cursor.next();

                Token::Other(c)
            }
        };

        Some(token)
    }
}
