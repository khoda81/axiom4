use std::{fs::File, io::Read};

use axiom4::{
    lexer::{self, Symbol},
    parser, tree,
};

fn main() {
    let mut file = File::open("tests/data/playground.cnf").expect("failed to open file");
    let mut buf = String::new();
    file.read_to_string(&mut buf).expect("failed to read file");

    let mut lexer = lexer::Lexer::new(&buf);
    // print_tokens(&mut lexer);
    let tokens: Vec<_> = (&mut lexer).collect();

    // eprintln!("{:?}", &lexer.interner);
    let mut parser = parser::Parser::new(lexer.interner);
    let mut input = tokens.as_slice();
    let input_size = input.len();

    // Skip all the new line characters
    while let [lexer::Token::NewLine, rest @ ..] = input {
        input = rest;
    }

    while let Ok((rest, conjunction)) = parser.parse_conjunction(input) {
        input = rest;

        for (is_negated, clause) in conjunction {
            if is_negated {
                eprint!("! ")
            } else {
                eprint!("| ");
            }

            print_tree(clause, &parser.tree_interner, &parser.string_interner);
            eprint!(" ");
        }

        eprintln!();
    }

    println!(
        "parsed {} tokens out of {}",
        input_size - input.len(),
        input_size
    )
}

fn print_tree(
    node_id: tree::NodeId,
    tree_interner: &tree::TreeInterner,
    string_interner: &lexer::StringInterner,
) {
    match tree_interner.resolve(node_id) {
        tree::Node::Leaf(leaf) => {
            let symbol = Symbol::try_from_usize(leaf).expect("failed to make symbol from leaf");
            let leaf_name = string_interner
                .resolve(symbol)
                .expect("could not find leaf symbol");

            eprint!("{leaf_name}");
        }
        tree::Node::BinaryOperator(operator) => {
            let symbol =
                Symbol::try_from_usize(operator).expect("failed to make symbol from operator");

            let operator_name = string_interner
                .resolve(symbol)
                .expect("could not find operator symbol");

            eprint!("{operator_name}(");
            let left = tree_interner.left_child(node_id);
            print_tree(left, tree_interner, string_interner);
            eprint!(", ");
            let right = tree_interner.right_child(node_id);
            print_tree(right, tree_interner, string_interner);
            eprint!(")");
        }
        tree::Node::Reference(_) => { /* Ignore, should be unreachable. */ }
    }
}

fn print_tokens(lexer: &mut lexer::Lexer) {
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
