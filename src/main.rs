use std::{fs::File, io::Read};

use axiom4::lexer;

fn main() {
    let mut file = File::open("tests/data/playground.cnf").expect("failed to open file");
    let mut buf = String::new();
    file.read_to_string(&mut buf).expect("failed to read file");

    let mut lexer = lexer::Lexer::new(&buf);
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

    eprintln!("{:?}", lexer.interner);
}
