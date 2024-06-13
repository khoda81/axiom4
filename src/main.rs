use axiom4::{cnf, lexer, parser, tree};
use std::{fmt, fs};
use std::{fmt::Display as _, io::Read as _};

fn main() {
    let show_all_clauses = true;
    let show_remainder = true;
    let show_interned_trees = false;
    let show_interned_strings = false;

    let print_variable_scopes = false;
    let cnf_path = "tests/data/base.axm";

    let mut file = fs::File::open(cnf_path).expect("failed to open file");
    let mut buf = String::new();
    file.read_to_string(&mut buf).expect("failed to read file");

    let mut lexer = lexer::Lexer::new(&buf);
    let tokens: Vec<_> = (&mut lexer).collect();

    let mut parser = parser::Parser::new(lexer.interner);

    let input = tokens.as_slice();
    let (rest, cnf) = parser.parse_cnf(input).expect("failed to parse cnf");

    for conjunction in cnf.iter() {
        let formatter = conjunction.format(&parser.tree_interner, &parser.string_interner);
        eprintln!("{formatter}");
    }

    if show_remainder {
        eprintln!();
        eprintln!("Remainder: {}/{} Tokens", rest.len(), input.len());
        print_tokens(rest.iter().copied(), &parser.string_interner);
    }

    if show_all_clauses {
        print_all_clauses(&cnf, &parser);
    }

    eprintln!();
    let p_clause_index = 15;
    let n_clause_index = 4;

    let positive_clause = cnf.clauses(cnf::Sign::Positive)[p_clause_index];
    let p_conjunction = cnf.find_conjunction(p_clause_index, cnf::Sign::Positive);
    let p_conjunction_ref = cnf.conjunction_ref(p_conjunction).unwrap();

    eprintln!(
        "Positive[{p_clause_index}]{}",
        format_highlighted(
            p_conjunction_ref,
            positive_clause,
            cnf::Sign::Positive,
            &parser
        )
    );

    let negative_clause = cnf.clauses(cnf::Sign::Negative)[n_clause_index];
    let n_conjunction = cnf.find_conjunction(n_clause_index, cnf::Sign::Negative);
    let n_conjunction_ref = cnf.conjunction_ref(n_conjunction).unwrap();
    eprintln!(
        "Negative[{p_clause_index}]{}",
        format_highlighted(
            n_conjunction_ref,
            negative_clause,
            cnf::Sign::Negative,
            &parser
        )
    );

    let mut conclusion = conclude(
        &mut parser,
        positive_clause,
        negative_clause,
        p_conjunction_ref,
        n_conjunction_ref,
        print_variable_scopes,
    );

    eprintln!();
    eprintln!("Conclusion:");
    eprintln!(
        "{}",
        conclusion
            .as_conjunction_ref()
            .format(&parser.tree_interner, &parser.string_interner)
    );

    if show_interned_trees {
        eprintln!();
        eprintln!("Interned Trees: ");
        for (idx, node) in parser.tree_interner.iter_nodes().enumerate() {
            eprint!("{idx}: ");

            let node = match node {
                Err(node_id) => {
                    eprint!("^{node_id:?} -> ");
                    parser.tree_interner.resolve_internal(node_id)
                }
                Ok(node) => node,
            };

            use crate::tree::interner::InternalNode;
            let symbol = match node {
                InternalNode::BinaryOperator(symbol) | InternalNode::Term(symbol) => symbol,
                InternalNode::Variable { id } => parser.tree_interner.resolve_variable_symbol(id),
            };

            let node_name = parser
                .string_interner
                .resolve(symbol)
                .expect("could not find symbol");

            match node {
                InternalNode::Term(_) => eprintln!("trm({node_name})"),
                InternalNode::BinaryOperator(_) => eprintln!("bop({node_name})"),
                InternalNode::Variable { id } => {
                    if print_variable_scopes {
                        let scope = parser.tree_interner.resolve_variable_scope(id);
                        eprintln!("{{{node_name}_{scope}}}");
                    } else {
                        eprintln!("{{{node_name}}}");
                    }
                }
            }
        }
    }

    if show_interned_strings {
        eprintln!();
        eprintln!("Interned Strings: ");
        eprintln!("{:?}", parser.string_interner.pool());
    }
}

fn conclude(
    parser: &mut parser::Parser,
    positive_clause: tree::NodeId,
    negative_clause: tree::NodeId,
    p_conjunction_ref: cnf::ConjunctionRef<'_>,
    n_conjunction_ref: cnf::ConjunctionRef<'_>,
    print_variable_scopes: bool,
) -> cnf::Conjunction {
    let matcher = tree::matcher::Matcher::new(&parser.tree_interner);
    let matcher = matcher
        .r#match(positive_clause, negative_clause)
        .expect("did not match");

    print_matcher(&matcher, parser, print_variable_scopes);

    let mut r#match = matcher.finish();
    let instance = r#match
        .instantiate(positive_clause, &mut parser.tree_interner)
        .unwrap();

    let mut conclusion = cnf::Conjunction::new();

    p_conjunction_ref
        .negatives
        .iter()
        .chain(n_conjunction_ref.negatives)
        .map(|&clause| {
            r#match
                .instantiate(clause, &mut parser.tree_interner)
                .unwrap()
        })
        .filter(|&clause| clause != instance)
        .for_each(|clause| conclusion.push(cnf::Sign::Negative, clause));

    p_conjunction_ref
        .positives
        .iter()
        .chain(n_conjunction_ref.positives)
        .map(|&clause| {
            r#match
                .instantiate(clause, &mut parser.tree_interner)
                .unwrap()
        })
        .filter(|&clause| clause != instance)
        .for_each(|clause| conclusion.push(cnf::Sign::Positive, clause));

    conclusion
}

fn print_all_clauses(cnf: &cnf::CNF, parser: &parser::Parser) {
    eprintln!();
    eprintln!("Positives: ");
    print_clauses(cnf, parser, cnf::Sign::Positive);

    eprintln!();
    eprintln!("Negatives: ");
    print_clauses(cnf, parser, cnf::Sign::Negative);
}

fn print_clauses(cnf: &cnf::CNF, parser: &parser::Parser, sign: cnf::Sign) {
    let clauses = cnf.clauses(sign);
    for (idx, &selected_clause) in clauses.iter().enumerate() {
        let conjunction_index = cnf.find_conjunction(idx, sign);
        let conjunction = cnf.conjunction_ref(conjunction_index).unwrap();
        let conjunction_formatter = format_highlighted(conjunction, selected_clause, sign, parser);

        eprintln!("{idx}{conjunction_formatter}");
    }
}

fn format_highlighted<'a>(
    conjunction: cnf::ConjunctionRef<'a>,
    selected_clause: tree::NodeId,
    sign: cnf::Sign,
    parser: &'a parser::Parser,
) -> cnf::ConjunctionFormatter<
    'a,
    impl Fn(tree::NodeId, cnf::Sign, &mut fmt::Formatter<'_>) -> fmt::Result + 'a,
> {
    const HIGHLIGHT: &str = "\x1b[30;43m";
    const CLEAR: &str = "\x1b[0m";

    cnf::ConjunctionFormatter {
        conjunction,
        first_side: sign,
        format_clause: move |clause: tree::NodeId, clause_sign, f: &mut fmt::Formatter| {
            let mut formatter = clause.format(&parser.tree_interner, &parser.string_interner);
            formatter.parent_precedence = parser::precedences::LOGIC_OR;
            let is_selected = selected_clause == clause && clause_sign == sign;

            if is_selected {
                f.write_str(HIGHLIGHT)?;
                formatter.fmt(f)?;
                f.write_str(CLEAR)
            } else {
                formatter.fmt(f)
            }
        },
    }
}

fn print_matcher(
    matcher: &tree::matcher::Matcher<'_>,
    parser: &parser::Parser,
    print_variable_scopes: bool,
) {
    eprintln!();
    eprintln!("Bindings: ");
    for variable_set in matcher.bindings().into_section_vec().iter_sections() {
        eprint!("{{ ");
        for &variable_id in variable_set {
            let symbol = parser.tree_interner.resolve_variable_symbol(variable_id);
            let name = parser.string_interner.resolve(symbol).unwrap();

            if print_variable_scopes {
                let scope = parser.tree_interner.resolve_variable_scope(variable_id);
                eprint!("{name}_{scope} ");
            } else {
                eprint!("{name} ");
            }
        }
        eprintln!("}}");
    }

    eprintln!("Assignments: ");
    for (&variable_id, &tree) in matcher.assignments().iter() {
        let symbol = parser.tree_interner.resolve_variable_symbol(variable_id);
        let name = parser.string_interner.resolve(symbol).unwrap();
        eprintln!("{name}: {}", parser.format_tree(tree));
    }
}

pub fn print_tokens(
    tokens: impl IntoIterator<Item = lexer::Token>,
    interner: &lexer::interner::StringInterner,
) {
    for token in tokens {
        match token {
            lexer::Token::Symbol(symbol) => {
                let symbol_name = interner.resolve(symbol).expect("failed for find symbol");
                eprint!("{symbol_name:?} ");
            }

            token => eprint!("{token:?} "),
        }
    }
}
