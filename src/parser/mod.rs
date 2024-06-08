use crate::{
    cnf::{Conjunction, Sign, CNF},
    lexer::{
        interner::{StringInterner, Symbol},
        Token,
    },
    tree::{
        interner::{TreeFormatter, TreeInterner},
        NodeId,
    },
};
use nom::{
    error::ErrorKind,
    multi::many1,
    sequence::{delimited, pair},
    Err, IResult, Parser as _,
};

pub mod utils;

pub mod precedences {
    pub const LOGIC_OR: u16 = 12;
    pub const COMP: u16 = 9;
    pub const ADDITION: u16 = 4;
    pub const UNARY: u16 = 4;
}

pub mod names {
    pub const UNARY: &str = "unary_operator";

    pub const ADD: &str = "add";
    pub const NEG: &str = "neg";
    pub const MUL: &str = "mul";
    pub const DIV: &str = "div";

    pub const EQ: &str = "equals";
    pub const LT: &str = "is_less_than";
}

pub enum ParseError {
    UnmatchedToken {
        expected_token: Token,
        received_token: Token,
    },
    UnexpectedEOF,
}

/// An LL(1) parser for FOL in CNF format.
///
/// S       -> Conj* 'NL'*
/// Conj    -> 'NL'* Clause ('Bar' Clause)*
/// Clause  -> Expr 'NEq' Expr | Expr 'Eq' Expr
/// Expr    -> Term (('+' | '-') Term)*
/// Term    -> Factor (('*' | '/') Factor)
/// Factor  -> Identifier | '(' Expr ')'
#[derive(Clone, Debug)]
pub struct Parser {
    pub tree_interner: TreeInterner,
    pub string_interner: StringInterner,
}

type Clause = (Sign, NodeId);

impl Parser {
    pub fn new(string_interner: StringInterner) -> Self {
        Self::with_interner(TreeInterner::new(), string_interner)
    }

    pub fn with_interner(tree_interner: TreeInterner, string_interner: StringInterner) -> Self {
        Self {
            tree_interner,
            string_interner,
        }
    }

    fn make_factor(&mut self, symbol: Symbol) -> NodeId {
        self.tree_interner
            .intern_term(symbol)
            .expect("failed to intern symbol as a factor")
    }

    fn make_variable(&mut self, symbol: Symbol) -> NodeId {
        self.tree_interner
            .intern_variable(symbol)
            .expect("failed to intern symbol as a variable")
    }

    fn make_binary_expression(&mut self, operator: Symbol, left: NodeId, right: NodeId) -> NodeId {
        self.tree_interner
            .intern_operator(operator, left, right)
            .expect("failed to intern a binary operator")
    }

    fn make_unary_expression(&mut self, operator: Symbol, inner: NodeId) -> NodeId {
        let operator_node = self.make_factor(operator);
        let unary = self.string_interner.intern(names::UNARY);

        self.make_binary_expression(unary, inner, operator_node)
    }

    pub fn parse_unary_function_argument<'a>(
        &mut self,
        input: &'a [Token],
    ) -> IResult<&'a [Token], NodeId> {
        let open_paren = utils::take_if(|&t: &Token| t == Token::OpenParen);
        let close_paren = utils::take_if(|&t: &Token| t == Token::CloseParen);
        delimited(open_paren, |inp| self.parse_expression(inp), close_paren)(input)
    }

    pub fn parse_binary_function_arguments<'a>(
        &mut self,
        input: &'a [Token],
    ) -> IResult<&'a [Token], (NodeId, NodeId)> {
        let args = |input| {
            let (input, o1) = self.parse_expression(input)?;
            let (input, _) = utils::take_if(|&t: &Token| t == Token::Comma)(input)?;
            self.parse_expression(input).map(|(i, o2)| (i, (o1, o2)))
        };

        let open_paren = utils::take_if(|&t: &Token| t == Token::OpenParen);
        let close_paren = utils::take_if(|&t: &Token| t == Token::CloseParen);
        delimited(open_paren, args, close_paren)(input)
    }

    pub fn parse_factor<'a>(&mut self, input: &'a [Token]) -> IResult<&'a [Token], NodeId> {
        if let [Token::Symbol(symbol), rest @ ..] = input {
            let result = if let Ok((rest, argument)) = self.parse_unary_function_argument(rest) {
                // Parse unary function
                (rest, self.make_unary_expression(*symbol, argument))
            } else if let Ok((rest, (left, right))) = self.parse_binary_function_arguments(rest) {
                // Parse binary function
                (rest, self.make_binary_expression(*symbol, left, right))
            } else {
                // NVM, its a normal identifier
                (rest, self.make_factor(*symbol))
            };

            return Ok(result);
        }

        if let [Token::OpenBrace, Token::Symbol(symbol), Token::CloseBrace, rest @ ..] = input {
            let variable = self.make_variable(*symbol);
            return Ok((rest, variable));
        }

        if let [Token::Minus, rest @ ..] = input {
            let (rest, inner_node) = self.parse_factor(rest)?;

            let neg = self.string_interner.intern(names::NEG);
            let node_id = self.make_unary_expression(neg, inner_node);

            return Ok((rest, node_id));
        }

        self.parse_unary_function_argument(input)
    }

    pub fn parse_term<'a>(&mut self, input: &'a [Token]) -> IResult<&'a [Token], NodeId> {
        let (mut rest, mut term) = self.parse_factor(input)?;
        let div_symbol = self.string_interner.intern(names::DIV);
        let mul_symbol = self.string_interner.intern(names::MUL);

        loop {
            let parse_operator =
                utils::take_if(|&t: &Token| matches!(t, Token::Star | Token::Slash));

            let len = rest.len();
            let parse_result = pair(parse_operator, |inp| self.parse_factor(inp))(rest);

            match parse_result {
                Err(nom::Err::Error(_)) => return Ok((rest, term)),
                Err(e) => return Err(e),
                Ok((new_rest, (&operator, factor))) => {
                    rest = new_rest;

                    // infinite loop check: the parser must always consume
                    if rest.len() == len {
                        return Err(Err::Error(nom::error::Error::new(rest, ErrorKind::Many0)));
                    }

                    let operator = match operator {
                        Token::Slash => div_symbol,
                        Token::Star => mul_symbol,
                        _ => unreachable!("token should be either slash or star"),
                    };

                    term = self.make_binary_expression(operator, term, factor);
                }
            }
        }
    }

    pub fn parse_expression<'a>(&mut self, input: &'a [Token]) -> IResult<&'a [Token], NodeId> {
        let (mut rest, mut expression) = self.parse_term(input)?;
        let neg_symbol = self.string_interner.intern(names::NEG);
        let add_symbol = self.string_interner.intern(names::ADD);

        loop {
            let parse_operator =
                utils::take_if(|&t: &Token| matches!(t, Token::Minus | Token::Plus));

            let len = rest.len();
            let parse_result = pair(parse_operator, |inp| self.parse_term(inp))(rest);

            rest = match parse_result {
                Err(nom::Err::Error(_)) => return Ok((rest, expression)),
                Err(e) => return Err(e),
                Ok((rest, (&operator, mut term))) => {
                    // infinite loop check: the parser must always consume
                    if rest.len() == len {
                        return Err(Err::Error(nom::error::Error::new(rest, ErrorKind::Many0)));
                    }

                    if let Token::Minus = operator {
                        // term <- neg(term)
                        term = self.make_unary_expression(neg_symbol, term);
                    }

                    expression = self.make_binary_expression(add_symbol, expression, term);
                    rest
                }
            }
        }
    }

    pub fn parse_clause<'a>(&mut self, input: &'a [Token]) -> IResult<&'a [Token], Clause> {
        let (rest, mut left_side) = self.parse_expression(input)?;
        let eq_symbol = self.string_interner.intern(names::EQ);
        let lt_symbol = self.string_interner.intern(names::LT);

        let parse_operator = utils::take_if(|&t: &Token| {
            matches!(
                t,
                Token::Eq | Token::NEq | Token::Lt | Token::LEq | Token::Gt | Token::GEq
            )
        });

        let parse_result = pair(parse_operator, |inp| self.parse_expression(inp))(rest);

        match parse_result {
            Err(nom::Err::Error(_)) => Ok((rest, (Sign::Positive, left_side))),
            Err(e) => Err(e),
            Ok((rest, (&operator, mut right_side))) => {
                let (sign, operator) = match operator {
                    Token::Eq => (Sign::Positive, eq_symbol),
                    Token::NEq => (Sign::Negative, eq_symbol),

                    Token::Lt => (Sign::Positive, lt_symbol),
                    Token::Gt => {
                        (right_side, left_side) = (left_side, right_side);
                        (Sign::Positive, lt_symbol)
                    }

                    Token::GEq => (Sign::Negative, lt_symbol),
                    Token::LEq => {
                        (right_side, left_side) = (left_side, right_side);
                        (Sign::Negative, lt_symbol)
                    }

                    _ => unreachable!(),
                };

                let clause = self.make_binary_expression(operator, left_side, right_side);

                Ok((rest, (sign, clause)))
            }
        }
    }

    pub fn parse_conjunction<'a>(
        &mut self,
        input: &'a [Token],
    ) -> IResult<&'a [Token], Conjunction> {
        // Skip all the new line characters
        let (input, _num_lines) = utils::eat_semicolon(input)?;

        let mut conjunction = Conjunction::new();

        let mut rest = match self.parse_clause(input) {
            Ok((rest, (sign, clause))) => {
                conjunction.push(sign, clause);
                rest
            }
            Err(nom::Err::Error(_)) => input,
            Err(e) => return Err(e),
        };

        loop {
            let parse_operator =
                utils::take_if(|&t: &Token| matches!(t, Token::Bar | Token::Bang)).map(|s| *s);

            let len = rest.len();
            let parse_result = pair(many1(parse_operator), |inp| self.parse_clause(inp))(rest);

            rest = match parse_result {
                Err(nom::Err::Error(_)) => {
                    if conjunction.is_empty() {
                        return Err(Err::Error(nom::error::Error::new(rest, ErrorKind::Many1)));
                    }

                    self.tree_interner.push_scope();
                    return Ok((rest, conjunction));
                }
                Err(e) => return Err(e),
                Ok((rest, (operators, (mut sign, clause)))) => {
                    // infinite loop check: the parser must always consume
                    if rest.len() == len {
                        return Err(Err::Error(nom::error::Error::new(rest, ErrorKind::Many0)));
                    }

                    operators
                        .into_iter()
                        .filter(|&op| op == Token::Bang)
                        .for_each(|_| sign = !sign);

                    conjunction.push(sign, clause);
                    rest
                }
            }
        }
    }

    pub fn parse_cnf<'a>(&mut self, mut input: &'a [Token]) -> IResult<&'a [Token], CNF> {
        let mut cnf = CNF::new();
        loop {
            input = match self.parse_conjunction(input) {
                Err(nom::Err::Error(_)) => break,
                Err(e) => return Err(e),
                Ok((rest, conjunction)) => {
                    cnf.assert(conjunction);
                    rest
                }
            }
        }

        let (input, _) = utils::eat_semicolon(input)?;
        Ok((input, cnf))
    }

    pub fn format_tree(&self, node_id: NodeId) -> TreeFormatter {
        let mut formatter = node_id.format(&self.tree_interner, &self.string_interner);
        formatter.parent_precedence = 12;
        formatter
    }
}

impl Default for Parser {
    fn default() -> Self {
        Self::new(StringInterner::new())
    }
}
