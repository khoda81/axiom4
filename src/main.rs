use axiom4::{lexer, parser};
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

    while let Ok((rest, conjunction)) = parser.parse_conjunction(input) {
        input = rest;

        for (is_negated, clause) in conjunction {
            if is_negated {
                eprint!("! ")
            } else {
                eprint!("| ");
            }

            parser.print_tree(clause);
            eprint!(" ");
        }

        eprintln!();
    }

    let (input, _) = parser::utils::eat_newline(input).expect("failed to eat newlines");

    println!(
        "parsed {} tokens out of {}",
        input_size - input.len(),
        input_size
    )
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
