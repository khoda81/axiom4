use axiom4::{lexer, parser, tree};
use std::{fs::File, io::Read};

fn main() {
    let mut file = File::open("tests/data/playground.cnf").expect("failed to open file");
    let mut buf = String::new();
    file.read_to_string(&mut buf).expect("failed to read file");

    let mut lexer = lexer::Lexer::new(&buf);
    let tokens: Vec<_> = (&mut lexer).collect();

    let mut parser = parser::Parser::new(lexer.interner);

    let input = tokens.as_slice();
    let (rest, cnf) = parser.parse_cnf(input).expect("failed to parse cnf");

    for conjunction in cnf.iter() {
        for &clause in conjunction.negatives {
            eprint!(" ! ");
            parser.print_tree(clause);
        }

        for &clause in conjunction.positives {
            eprint!(" | ");
            parser.print_tree(clause);
        }

        eprintln!();
    }

    eprintln!();
    eprintln!("Positives: ");
    for (idx, &clause) in cnf.positive_clauses().items().iter().enumerate() {
        eprint!("{idx}: ");
        parser.print_tree(clause);
        eprintln!();
    }

    eprintln!();
    eprintln!("Negatives: ");
    for (idx, &clause) in cnf.negative_clauses().items().iter().enumerate() {
        eprint!("{idx}: ");
        parser.print_tree(clause);
        eprintln!();
    }

    eprintln!();
    let positive_idx = 19;
    let negative_idx = 10;

    let positive_clause = cnf.positive_clauses().items()[positive_idx];
    eprint!("Positive[{positive_idx}]: ");
    parser.print_tree(positive_clause);
    eprintln!();

    let negative_clause = cnf.negative_clauses().items()[negative_idx];
    eprint!("Negative[{negative_idx}]: ");
    parser.print_tree(negative_clause);
    eprintln!();

    let matcher = tree::matcher::Matcher::new(&parser.tree_interner);
    let matcher = matcher
        .r#match(positive_clause, negative_clause)
        .expect("did not match");

    eprintln!();
    eprint!("Bindings: ");
    for symbol_set in matcher.bindings().into_section_vec().iter_sections() {
        eprintln!();
        for &symbol in symbol_set {
            let name = parser.string_interner.resolve(symbol).unwrap();
            eprint!("{name} ");
        }
    }

    eprintln!();
    eprint!("Assignments: ");
    for (&symbol, &tree) in matcher.assignments().iter() {
        eprintln!();
        let name = parser.string_interner.resolve(symbol).unwrap();
        eprint!("{name}: ");
        parser.print_tree(tree);
    }
    eprintln!();

    eprintln!();
    let mut r#match = matcher.finish();

    let positive_instance = r#match.instantiate(positive_clause, &mut parser.tree_interner);
    eprint!("positive: ");
    parser.print_tree(positive_instance.unwrap());
    eprintln!();

    let negative_instance = r#match.instantiate(negative_clause, &mut parser.tree_interner);
    eprint!("negative: ");
    parser.print_tree(negative_instance.unwrap());
    eprintln!();

    eprintln!();
    eprintln!("Remainder: {}/{} Tokens", rest.len(), input.len());
    print_tokens(rest.iter().copied(), &parser.string_interner);

    eprintln!();
    eprintln!("Interned Trees: ");
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

    eprintln!();
    eprintln!("Interned Strings: ");
    eprintln!("{:?}", parser.string_interner.pool());
}

pub fn print_tokens(
    mut tokens: impl Iterator<Item = lexer::Token>,
    interner: &lexer::interner::StringInterner,
) {
    loop {
        let Some(token) = tokens.next() else { break };
        match token {
            lexer::Token::Symbol(symbol) => {
                let symbol_name = interner.resolve(symbol).expect("failed for find symbol");
                eprint!("{symbol_name:?} ");
            }

            lexer::Token::NewLine => eprintln!("{token:?}"),
            token => eprint!("{token:?} "),
        }
    }
}
