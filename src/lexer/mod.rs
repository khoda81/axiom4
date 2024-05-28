use std::{collections::HashMap, num::NonZeroU32, str::Chars};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Symbol(NonZeroU32);

#[derive(Debug, Default)]
pub struct Interner {
    pool: HashMap<String, Symbol>,
}

impl Interner {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn find(&mut self, name: &str) -> Option<Symbol> {
        self.pool.get(name).copied()
    }

    pub fn intern(&mut self, name: &str) -> Symbol {
        if let Some(symbol) = self.find(name) {
            return symbol;
        }

        let symbol_id = NonZeroU32::new((self.pool.len() + 1) as u32).unwrap();
        self.pool.insert(name.to_string(), Symbol(symbol_id));
        Symbol(symbol_id)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Symbol(Symbol),
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

#[derive(Debug)]
pub struct Lexer<'a> {
    interner: Interner,
    cursor: Chars<'a>,
}

impl<'a> Lexer<'a> {
    pub fn new(text: &'a str) -> Self {
        Self::new_with_interner(text, Interner::new())
    }

    fn new_with_interner(text: &'a str, interner: Interner) -> Self {
        Self {
            interner,
            cursor: text.chars(),
        }
    }

    pub fn interner(&self) -> &Interner {
        &self.interner
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

        if next_char.is_ascii_alphanumeric() {
            let next_token = text
                .find(|c: char| !c.is_ascii_alphanumeric())
                .unwrap_or(text.len());

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
