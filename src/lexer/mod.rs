use std::{collections::HashMap, num::NonZeroU32, rc::Rc, str::Chars};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol(NonZeroU32);

impl Symbol {
    pub fn as_usize(self) -> usize {
        self.0.get() as usize
    }

    pub fn try_from_usize(value: usize) -> Option<Self> {
        NonZeroU32::new(value as u32).map(Symbol)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StringInterner {
    pool: HashMap<Rc<str>, Symbol>,
    reverse_pool: Vec<Rc<str>>,
}

impl StringInterner {
    pub fn new() -> Self {
        Self {
            pool: HashMap::new(),
            reverse_pool: vec![Rc::from("")],
        }
    }

    pub fn find(&self, name: &str) -> Option<Symbol> {
        self.pool.get(name).copied()
    }

    pub fn resolve(&self, symbol: Symbol) -> Option<&str> {
        self.reverse_pool
            .get(symbol.0.get() as usize)
            .map(|rc_str| &**rc_str)
    }

    pub fn intern(&mut self, name: &str) -> Symbol {
        if let Some(&symbol) = self.pool.get(name) {
            return symbol;
        }

        let symbol_id = NonZeroU32::new(self.reverse_pool.len() as u32)
            .expect("the length of reverse pool should not be 0 or exceed u32::MAX");

        let rc_name: Rc<str> = Rc::from(name);
        self.pool.insert(rc_name.clone(), Symbol(symbol_id));
        self.reverse_pool.push(rc_name);
        Symbol(symbol_id)
    }
}

impl Default for StringInterner {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
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

#[derive(Clone, Debug)]
pub struct Lexer<'a> {
    pub interner: StringInterner,
    cursor: Chars<'a>,
}

impl<'a> Lexer<'a> {
    pub fn new(text: &'a str) -> Self {
        Self::new_with_interner(text, StringInterner::new())
    }

    fn new_with_interner(text: &'a str, interner: StringInterner) -> Self {
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
