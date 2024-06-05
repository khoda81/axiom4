use crate::{
    lexer::{
        interner::{StringInterner, Symbol},
        Token,
    },
    tree::{NodeId, NodeKind, TreeInterner},
};
use nom::{
    error::ErrorKind,
    multi::many1,
    sequence::{delimited, pair},
    Err, IResult, Parser as _,
};

pub mod utils;

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

type Clause = (bool, NodeId);

impl Parser {
    const UNARY: &'static str = "unary_operator";

    const ADD: &'static str = "add";
    const NEG: &'static str = "neg";
    const MUL: &'static str = "mul";
    const DIV: &'static str = "div";

    const EQ: &'static str = "equals";
    const LT: &'static str = "is_less_than";

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

    pub fn parse_factor<'a>(&mut self, input: &'a [Token]) -> IResult<&'a [Token], NodeId> {
        if let [Token::Symbol(symbol), rest @ ..] = input {
            let factor = self.make_factor(*symbol);
            return Ok((rest, factor));
        }

        if let [Token::Other('{'), Token::Symbol(symbol), Token::Other('}'), rest @ ..] = input {
            let variable = self.make_variable(*symbol);
            return Ok((rest, variable));
        }

        if let [Token::Minus, rest @ ..] = input {
            let (rest, inner_node) = self.parse_factor(rest)?;

            let neg = self.string_interner.intern(Self::NEG);
            let unary = self.string_interner.intern(Self::UNARY);

            let neg_node = self.make_factor(neg);
            let node_id = self.make_binary_expression(unary, inner_node, neg_node);

            return Ok((rest, node_id));
        }

        delimited(
            utils::take_if(|&t: &Token| t == Token::POpen),
            |inp| self.parse_expression(inp),
            utils::take_if(|&t: &Token| t == Token::PClose),
        )(input)
    }

    pub fn parse_term<'a>(&mut self, input: &'a [Token]) -> IResult<&'a [Token], NodeId> {
        let (mut rest, mut term) = self.parse_factor(input)?;
        let div_symbol = self.string_interner.intern(Self::DIV);
        let mul_symbol = self.string_interner.intern(Self::MUL);

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
        let neg_symbol = self.string_interner.intern(Self::NEG);
        let neg_node = self.make_factor(neg_symbol);

        let add_symbol = self.string_interner.intern(Self::ADD);
        let unary_symbol = self.string_interner.intern(Self::UNARY);

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
                        term = self.make_binary_expression(unary_symbol, term, neg_node);
                    }

                    expression = self.make_binary_expression(add_symbol, expression, term);
                    rest
                }
            }
        }
    }

    pub fn parse_clause<'a>(&mut self, input: &'a [Token]) -> IResult<&'a [Token], (bool, NodeId)> {
        let (rest, mut left_side) = self.parse_expression(input)?;
        let eq_symbol = self.string_interner.intern(Self::EQ);
        let lt_symbol = self.string_interner.intern(Self::LT);

        let parse_operator = utils::take_if(|&t: &Token| {
            matches!(
                t,
                Token::Eq | Token::NEq | Token::Lt | Token::LEq | Token::Gt | Token::GEq
            )
        });

        let parse_result = pair(parse_operator, |inp| self.parse_expression(inp))(rest);

        match parse_result {
            Err(nom::Err::Error(_)) => Ok((rest, (false, left_side))),
            Err(e) => Err(e),
            Ok((rest, (&operator, mut right_side))) => {
                let (is_negated, operator) = match operator {
                    Token::Eq => (false, eq_symbol),
                    Token::NEq => (true, eq_symbol),

                    Token::Lt => (false, lt_symbol),
                    Token::Gt => {
                        (right_side, left_side) = (left_side, right_side);
                        (false, lt_symbol)
                    }

                    Token::GEq => (true, lt_symbol),
                    Token::LEq => {
                        (right_side, left_side) = (left_side, right_side);
                        (true, lt_symbol)
                    }

                    _ => unreachable!(),
                };

                let clause = self.make_binary_expression(operator, left_side, right_side);

                Ok((rest, (is_negated, clause)))
            }
        }
    }

    pub fn parse_conjunction<'a>(
        &mut self,
        input: &'a [Token],
    ) -> IResult<&'a [Token], Vec<Clause>> {
        // Skip all the new line characters
        let (input, _num_lines) = utils::eat_newline(input)?;

        let (mut rest, mut conjunction) = match self.parse_clause(input) {
            Ok((rest, clause)) => (rest, vec![clause]),
            Err(nom::Err::Error(_)) => (input, vec![]),
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

                    return Ok((rest, conjunction));
                }
                Err(e) => return Err(e),
                Ok((rest, (operators, (mut is_neg, clause)))) => {
                    // infinite loop check: the parser must always consume
                    if rest.len() == len {
                        return Err(Err::Error(nom::error::Error::new(rest, ErrorKind::Many0)));
                    }

                    operators
                        .into_iter()
                        .filter(|&op| op == Token::Bang)
                        .for_each(|_| is_neg = !is_neg);

                    conjunction.push((is_neg, clause));
                    rest
                }
            }
        }
    }

    pub fn print_tree(&self, node_id: NodeId) {
        let node = self.tree_interner.resolve(node_id);
        let node_name = self
            .string_interner
            .resolve(node.symbol)
            .expect("could not find symbol");

        let inline_name = match node_name {
            Self::ADD => Some(" + "),
            Self::EQ => Some(" = "),
            Self::LT => Some(" < "),
            Self::NEG => Some("-"),
            _ => None,
        };

        match node.kind {
            NodeKind::Variable => eprint!("{{{node_name}}}"),
            NodeKind::Term => eprint!("{}", inline_name.unwrap_or(node_name)),
            NodeKind::BinaryOperator => {
                let left = self.tree_interner.left_child(node_id);
                let right = self.tree_interner.right_child(node_id);

                if node_name == Self::UNARY {
                    self.print_tree(right);
                    eprint!("(");
                    self.print_tree(left);
                } else if let Some(inline_name) = inline_name {
                    eprint!("(");
                    self.print_tree(left);
                    eprint!("{inline_name}");
                    self.print_tree(right);
                } else {
                    eprint!("{node_name}(");
                    self.print_tree(left);
                    eprint!(", ");
                    self.print_tree(right);
                }

                eprint!(")");
            }
        }
    }
}

impl Default for Parser {
    fn default() -> Self {
        Self::new(StringInterner::new())
    }
}
