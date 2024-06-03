use nom::{
    error::ErrorKind,
    sequence::{delimited, pair},
    Err, IResult,
};

use crate::{
    lexer::{StringInterner, Token},
    tree::{NodeId, TreeInterner},
};

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

    pub fn parse_factor<'a>(&mut self, input: &'a [Token]) -> IResult<&'a [Token], NodeId> {
        if let [Token::Symbol(symbol), rest @ ..] = input {
            // TODO: Treat free variables differently
            let leaf_value = symbol.as_usize();

            let node_id = self
                .tree_interner
                .intern_leaf(leaf_value)
                .expect("failed to intern symbol as a factor");

            return Ok((rest, node_id));
        }

        if let [Token::Minus, rest @ ..] = input {
            // TODO: Treat free variables differently
            let (rest, inner_node) = self.parse_factor(rest)?;

            let neg_symbol = self.string_interner.intern(Self::NEG);
            let unary_symbol = self.string_interner.intern(Self::UNARY);

            let neg_node = self
                .tree_interner
                .intern_leaf(neg_symbol.as_usize())
                .expect("failed to intern a neg");

            let node_id = self
                .tree_interner
                .intern_operator(unary_symbol.as_usize(), inner_node, neg_node)
                .expect("failed to intern a binary operator");

            return Ok((rest, node_id));
        }

        delimited(
            take_if(|&t: &Token| t == Token::POpen),
            |inp| self.parse_expression(inp),
            take_if(|&t: &Token| t == Token::PClose),
        )(input)
    }

    pub fn parse_term<'a>(&mut self, input: &'a [Token]) -> IResult<&'a [Token], NodeId> {
        let (mut rest, mut term) = self.parse_factor(input)?;
        let div_symbol = self.string_interner.intern(Self::DIV);
        let mul_symbol = self.string_interner.intern(Self::MUL);

        loop {
            let parse_operator = take_if(|&t: &Token| matches!(t, Token::Star | Token::Slash));

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

                    term = self
                        .tree_interner
                        .intern_operator(operator.as_usize(), term, factor)
                        .expect("failed to intern a binary operator");
                }
            }
        }
    }

    pub fn parse_expression<'a>(&mut self, input: &'a [Token]) -> IResult<&'a [Token], NodeId> {
        let (mut rest, mut expression) = self.parse_term(input)?;
        let neg_symbol = self.string_interner.intern(Self::NEG);
        let neg_node = self
            .tree_interner
            .intern_leaf(neg_symbol.as_usize())
            .expect("failed to intern a neg");

        let add_symbol = self.string_interner.intern(Self::ADD);
        let unary_symbol = self.string_interner.intern(Self::UNARY);

        loop {
            let parse_operator = take_if(|&t: &Token| matches!(t, Token::Minus | Token::Plus));

            let len = rest.len();
            let parse_result = pair(parse_operator, |inp| self.parse_term(inp))(rest);

            match parse_result {
                Err(nom::Err::Error(_)) => return Ok((rest, expression)),
                Err(e) => return Err(e),
                Ok((new_rest, (&operator, mut term))) => {
                    rest = new_rest;

                    // infinite loop check: the parser must always consume
                    if rest.len() == len {
                        return Err(Err::Error(nom::error::Error::new(rest, ErrorKind::Many0)));
                    }

                    if let Token::Minus = operator {
                        // term <- neg(term)
                        term = self
                            .tree_interner
                            .intern_operator(unary_symbol.as_usize(), term, neg_node)
                            .expect("failed to intern a binary operator");
                    };

                    expression = self
                        .tree_interner
                        .intern_operator(add_symbol.as_usize(), expression, term)
                        .expect("failed to intern a binary operator");
                }
            }
        }
    }

    pub fn parse_clause<'a>(&mut self, input: &'a [Token]) -> IResult<&'a [Token], (bool, NodeId)> {
        let (rest, mut left_side) = self.parse_expression(input)?;
        let eq_symbol = self.string_interner.intern(Self::EQ);
        let lt_symbol = self.string_interner.intern(Self::LT);

        let parse_operator = take_if(|&t: &Token| {
            matches!(
                t,
                Token::Eq | Token::NEq | Token::Lt | Token::LEq | Token::Gt | Token::GEq
            )
        });

        let parse_result = pair(parse_operator, |inp| self.parse_term(inp))(rest);

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

                let clause = self
                    .tree_interner
                    .intern_operator(operator.as_usize(), left_side, right_side)
                    .expect("failed to intern a binary operator");

                Ok((rest, (is_negated, clause)))
            }
        }
    }

    pub fn parse_conjunction<'a>(
        &mut self,
        mut input: &'a [Token],
    ) -> IResult<&'a [Token], Vec<Clause>> {
        // Skip all the new line characters
        while let [Token::NewLine, rest @ ..] = input {
            input = rest;
        }

        let (mut rest, first_clause) = self.parse_clause(input)?;
        let mut conjunction = vec![first_clause];

        loop {
            let parse_operator = take_if(|&t: &Token| matches!(t, Token::Bar));

            let len = rest.len();
            let parse_result = pair(parse_operator, |inp| self.parse_clause(inp))(rest);

            match parse_result {
                Err(nom::Err::Error(_)) => return Ok((rest, conjunction)),
                Err(e) => return Err(e),
                Ok((new_rest, (_operator, clause))) => {
                    rest = new_rest;

                    // infinite loop check: the parser must always consume
                    if rest.len() == len {
                        return Err(Err::Error(nom::error::Error::new(rest, ErrorKind::Many0)));
                    }

                    conjunction.push(clause);
                }
            }
        }
    }
}

impl Default for Parser {
    fn default() -> Self {
        Self::new(StringInterner::new())
    }
}

// impl<'a, I, O, E, F> Parser<I, O, E> for F
// where
//   F: FnMut(I) -> IResult<I, O, E> + 'a,
// {
//   fn parse(&mut self, i: I) -> IResult<I, O, E> {
//     self(i)
//   }
// }

fn take_if<'a, T, F: FnMut(&T) -> bool>(
    mut f: F,
) -> impl nom::Parser<&'a [T], &'a T, nom::error::Error<&'a [T]>> {
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
