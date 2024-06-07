use std::fmt::Display;

use crate::{
    lexer::{interner::StringInterner, Token},
    tree::{
        interner::{ScopeId, TreeInterner},
        NodeId, NodeKind,
    },
};
use nom::multi::many0_count;
use nom::IResult;

use super::Parser;

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

#[must_use]
#[derive(Copy, Clone)]
pub struct PrintTree<'a> {
    pub node_id: NodeId,
    pub print_scopes: bool,
    pub tree_interner: &'a TreeInterner,
    pub string_interner: &'a StringInterner,
}

impl Display for PrintTree<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.write_node(self.node_id, f)
    }
}

impl<'a> PrintTree<'a> {
    fn write_node(
        &self,
        node_id: NodeId,
        f: &mut std::fmt::Formatter,
    ) -> Result<(), std::fmt::Error> {
        let node = self.tree_interner.resolve(node_id);
        let node_name = self
            .string_interner
            .resolve(node.symbol)
            .expect("could not find symbol");

        let inline_name = match node_name {
            Parser::ADD => Some(" + "),
            Parser::EQ => Some(" = "),
            Parser::LT => Some(" < "),
            Parser::NEG => Some("-"),
            _ => None,
        };

        match node.kind {
            NodeKind::Variable { scope } => {
                Self::print_variable(node_name, self.print_scopes.then_some(scope), f)
            }

            NodeKind::Term => write!(f, "{}", inline_name.unwrap_or(node_name)),
            NodeKind::BinaryOperator => {
                let right = Self {
                    node_id: self.tree_interner.right_child(node_id),
                    ..*self
                };
                let left = Self {
                    node_id: self.tree_interner.left_child(node_id),
                    ..*self
                };

                if node_name == Parser::UNARY {
                    write!(f, "{right}({left})")
                } else if let Some(inline_name) = inline_name {
                    write!(f, "({left}{inline_name}{right})")
                } else {
                    write!(f, "{node_name}({left}, {right})")
                }
            }
        }
    }
}

impl<'a> PrintTree<'a> {
    fn print_variable(
        node_name: &str,
        scope: Option<ScopeId>,
        f: &mut std::fmt::Formatter,
    ) -> Result<(), std::fmt::Error> {
        match scope {
            Some(scope) => write!(f, "{{{node_name}_{scope}}}"),
            None => write!(f, "{{{node_name}}}"),
        }
    }
}
