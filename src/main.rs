use axiom4::{lexer, parser, tree};
use std::{fs::File, io::Read};

fn main() {
    let mut file = File::open("tests/data/playground.cnf").expect("failed to open file");
    let mut buf = String::new();
    file.read_to_string(&mut buf).expect("failed to read file");

    let mut lexer = lexer::Lexer::new(&buf);
    let tokens: Vec<_> = (&mut lexer).collect();

    let mut parser = parser::Parser::new(lexer.interner);
    let mut input = tokens.as_slice();
    let input_size = input.len();

    while let Ok((rest, mut conjunction)) = parser.parse_conjunction(input) {
        input = rest;

        for clause in conjunction.drain_positive() {
            eprint!("| ");
            parser.print_tree(clause);
            eprint!(" ");
        }

        for clause in conjunction.drain_negatives() {
            eprint!("! ");
            parser.print_tree(clause);
            eprint!(" ");
        }

        eprintln!();
    }

    let (input, _) = parser::utils::eat_newline(input).expect("failed to eat newlines");

    println!();
    println!("Remainder: {}/{input_size} Tokens", input.len());

    println!();
    println!("Interned Trees: ");
    let nodes: Vec<_> = parser.tree_interner.iter_nodes().collect();
    for (idx, node) in parser.tree_interner.iter_nodes().enumerate() {
        eprint!("{idx}: ");

        let node = match node {
            Ok(node) => node,
            Err(index) => {
                eprint!("^{index} -> ");
                nodes[index].unwrap()
            }
        };

        let node_name = parser
            .string_interner
            .resolve(node.symbol)
            .expect("could not find symbol");

        match node.kind {
            tree::NodeKind::Term => eprintln!("trm({node_name})"),
            tree::NodeKind::Variable => eprintln!("var({node_name})"),
            tree::NodeKind::BinaryOperator => eprintln!("bop({node_name})"),
        }
    }

    println!();
    println!("Interned Strings: ");
    println!("{:?}", parser.string_interner.pool());
}

pub fn print_tokens(lexer: &mut lexer::Lexer) {
    loop {
        let Some(token) = lexer.next() else { break };
        match token {
            lexer::Token::Symbol(symbol) => {
                let symbol_name = lexer
                    .interner
                    .resolve(symbol)
                    .expect("failed for find symbol");

                eprint!("{symbol_name:?} ");
            }

            lexer::Token::NewLine => eprintln!("{token:?}"),
            token => eprint!("{token:?} "),
        }
    }
}
