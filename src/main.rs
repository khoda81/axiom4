use std::{fs::File, io::Read};

use axiom4::lexer;

fn main() {
    let mut file = File::open("tests/data/playground.cnf").expect("failed to open file");
    let mut buf = String::new();
    file.read_to_string(&mut buf).expect("failed to read file");

    let lexer = lexer::Lexer::new(&buf);
    for token in lexer {
        eprintln!("{token:?}");
    }
}
