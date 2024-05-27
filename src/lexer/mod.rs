use std::{collections::HashMap, str::Chars};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Symbol(u32);

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

        let new_symbol = Symbol(self.pool.len() as u32);
        self.pool.insert(name.to_string(), new_symbol);
        new_symbol
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Symbol(Symbol),
    Other(char),
    Comma,
    Colon,
    SemiColon,
    POpen,
    PClose,
}

#[derive(Debug)]
pub struct Lexer<'a> {
    interner: Interner,
    cursor: Chars<'a>,
}

impl<'a> Lexer<'a> {
    pub fn new(text: &'a str) -> Self {
        Self {
            interner: Interner::new(),
            cursor: text.chars(),
        }
    }

    pub fn interner(&self) -> &Interner {
        &self.interner
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let text = self.cursor.as_str().trim_start();
        self.cursor = text.chars();

        let token = match self.cursor.next()? {
            ':' => Token::Colon,
            ';' => Token::SemiColon,
            ',' => Token::Comma,
            '(' => Token::POpen,
            ')' => Token::PClose,

            c if c.is_ascii_alphanumeric() => {
                let next_token = text
                    .find(|c: char| !c.is_ascii_alphanumeric())
                    .unwrap_or(text.len());

                let (ident, rest) = text.split_at(next_token);
                self.cursor = rest.chars();

                // Intern symbol
                let symbol = self.interner.intern(ident);
                Token::Symbol(symbol)
            }

            c => Token::Other(c),
        };

        Some(token)
    }
}

#[cfg(test)]
mod test_lexer {
    use super::*;

    #[test]
    fn test_basic_symbols() {
        let input = "ForAll A B C";
        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.next(), Some(Token::Symbol(Symbol(0))));
        assert_eq!(lexer.next(), Some(Token::Symbol(Symbol(1))));
        assert_eq!(lexer.next(), Some(Token::Symbol(Symbol(2))));
        assert_eq!(lexer.next(), Some(Token::Symbol(Symbol(3))));
        assert_eq!(lexer.next(), None);

        assert_eq!(lexer.interner.find("ForAll"), Some(Symbol(0)));
        assert_eq!(lexer.interner.find("A"), Some(Symbol(1)));
        assert_eq!(lexer.interner.find("B"), Some(Symbol(2)));
        assert_eq!(lexer.interner.find("C"), Some(Symbol(3)));
    }

    #[test]
    fn test_special_characters() {
        let input = ",:;()";
        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.next(), Some(Token::Comma));
        assert_eq!(lexer.next(), Some(Token::Colon));
        assert_eq!(lexer.next(), Some(Token::SemiColon));
        assert_eq!(lexer.next(), Some(Token::POpen));
        assert_eq!(lexer.next(), Some(Token::PClose));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn test_mixed_input() {
        let input = "Union(A, B);";
        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.next(), Some(Token::Symbol(Symbol(0))));
        assert_eq!(lexer.next(), Some(Token::POpen));
        assert_eq!(lexer.next(), Some(Token::Symbol(Symbol(1))));
        assert_eq!(lexer.next(), Some(Token::Comma));
        assert_eq!(lexer.next(), Some(Token::Symbol(Symbol(2))));
        assert_eq!(lexer.next(), Some(Token::PClose));
        assert_eq!(lexer.next(), Some(Token::SemiColon));
        assert_eq!(lexer.next(), None);

        assert_eq!(lexer.interner.find("Union"), Some(Symbol(0)));
        assert_eq!(lexer.interner.find("A"), Some(Symbol(1)));
        assert_eq!(lexer.interner.find("B"), Some(Symbol(2)));
    }

    #[test]
    fn test_whitespace_handling() {
        let input = "   ForAll    A    B   ";
        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.next(), Some(Token::Symbol(Symbol(0))));
        assert_eq!(lexer.next(), Some(Token::Symbol(Symbol(1))));
        assert_eq!(lexer.next(), Some(Token::Symbol(Symbol(2))));
        assert_eq!(lexer.next(), None);

        assert_eq!(lexer.interner.find("ForAll"), Some(Symbol(0)));
        assert_eq!(lexer.interner.find("A"), Some(Symbol(1)));
        assert_eq!(lexer.interner.find("B"), Some(Symbol(2)));
    }

    #[test]
    fn test_other_characters() {
        let input = "ForAll A % B";
        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.next(), Some(Token::Symbol(Symbol(0))));
        assert_eq!(lexer.next(), Some(Token::Symbol(Symbol(1))));
        assert_eq!(lexer.next(), Some(Token::Other('%')));
        assert_eq!(lexer.next(), Some(Token::Symbol(Symbol(2))));
        assert_eq!(lexer.next(), None);

        assert_eq!(lexer.interner.find("ForAll"), Some(Symbol(0)));
        assert_eq!(lexer.interner.find("A"), Some(Symbol(1)));
        assert_eq!(lexer.interner.find("B"), Some(Symbol(2)));
    }
}
