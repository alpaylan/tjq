use std::{collections::HashMap, vec};

use tracing_subscriber::EnvFilter;
use tree_sitter::Node;

use crate::filter::Filter;
use crate::printer::{print_ast, print_node_details};

pub(crate) fn parse(code: &str) -> (HashMap<String, Filter>, Filter) {
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&tree_sitter_jq::LANGUAGE.into())
        .expect("Error loading jq grammar");
    let tree = parser.parse(code, None).unwrap();

    assert_eq!(tree.root_node().kind(), "program");

    print_ast(tree.root_node(), code, 0); // print AST
                                          //print_node_details(tree.root_node(), code, 0);

    let mut defs = HashMap::new();

    for i in 0..tree.root_node().child_count() - 1 {
        let child = tree.root_node().child(i).unwrap();
        match child.kind() {
            "function_definition" => {
                let f = parse_filter(code, child, &mut defs);
                tracing::trace!("Parsed function definition: {}", f);
            }
            "comment" => {}
            _ => {
                panic!("unexpected node in program: {}", child.kind());
            }
        }
    }
    let f = parse_filter(
        code,
        tree.root_node()
            .child(tree.root_node().child_count() - 1)
            .expect("root should have at one children"),
        &mut defs,
    );

    (defs, f)
}

pub(crate) fn parse_defs(code: &str) -> HashMap<String, Filter> {
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&tree_sitter_jq::LANGUAGE.into())
        .expect("Error loading jq grammar");
    let tree = parser.parse(code, None).unwrap();

    assert_eq!(tree.root_node().kind(), "program");

    let mut defs = HashMap::new();

    for i in 0..tree.root_node().child_count() {
        let child = tree.root_node().child(i).unwrap();
        match child.kind() {
            "function_definition" => {
                let _ = parse_filter(code, child, &mut defs);
            }
            "comment" => {}
            _ => {
                panic!("unexpected node in program: {}", child.kind());
            }
        }
    }

    defs
}

pub(crate) fn parse_filter<'a>(
    code: &str,
    root: Node<'a>,
    defs: &mut HashMap<String, Filter>,
) -> Filter {
    tracing::trace!(
        "{}: {}",
        root.kind(),
        &code[root.range().start_byte..root.range().end_byte]
    );
    match root.kind() {
        "dot" => Filter::Dot,
        "sequence_expression" => {
            let lhs = parse_filter(
                code,
                root.child(0).expect("sequence should have a lhs"),
                defs,
            );
            let rhs = parse_filter(
                code,
                root.child(2).expect("sequence should have a rhs"),
                defs,
            );
            Filter::Comma(Box::new(lhs), Box::new(rhs))
        }
        "subscript_expression" => {
            let lhs = parse_filter(
                code,
                root.child(0).expect("subscript should have a lhs"),
                defs,
            );

            let rhs = if root.child_count() == 3 {
                Filter::ArrayIterator
            } else {
                // todo: Implement expressions in the index position
                // Filter::ArrayIndex(parse_filter(
                //     code,
                //     root.child(2).expect("subscript should have a rhs"),
                //     defs,
                // ))
                let rhs = root.child(2).unwrap();
                let rhs = code[rhs.range().start_byte..rhs.range().end_byte].parse::<isize>().expect(format!("array index should be a valid integer(expressions in the index are not yet supported), encountered '{}'", &code[rhs.range().start_byte..rhs.range().end_byte]).as_str());
                Filter::ArrayIndex(rhs)
            };

            match lhs {
                Filter::Dot => rhs,
                _ => Filter::Pipe(Box::new(lhs), Box::new(rhs)),
            }
        }
        "field" => {
            // get the second child and make it an ObjectIndex
            let identifier = parse_filter(
                code,
                root.child(1)
                    .expect("field access should have the second child as its field"),
                defs,
            );
            Filter::ObjIndex(identifier.to_string())
        }
        "field_id" => {
            Filter::String(code[root.range().start_byte..root.range().end_byte].to_string())
        }
        "identifier" => match &code[root.range().start_byte..root.range().end_byte] {
            "true" => Filter::Boolean(true),
            "false" => Filter::Boolean(false),
            "null" => Filter::Null,
            "empty" => Filter::Empty,
            "error" => Filter::Error,
            s => Filter::Call(s.to_string(), None),
        },
        "variable" => {
            tracing::warn!("todo: variables are not yet supported");
            unimplemented!(
                "todo: variables are not yet supported, encountered '{}'",
                &code[root.range().start_byte..root.range().end_byte]
            );
        }
        "array" => {
            if root.child_count() == 2 {
                return Filter::Array(vec![]);
            }

            Filter::Array(
                (1..root.child_count())
                    .step_by(2)
                    .map(|i| {
                        parse_filter(
                            code,
                            root.child(i).expect("array should have a value"),
                            defs,
                        )
                    })
                    .collect(),
            )
        }
        "object" => {
            let mut pairs: Vec<(Filter, Filter)> = vec![];
            for i in (1..root.child_count() - 1).step_by(2) {
                let pair = root.child(i).unwrap();
                let key = parse_filter(code, pair.child(0).expect("pairs should have a key"), defs);
                let value = parse_filter(
                    code,
                    pair.child(2).expect("pairs should have a value"),
                    defs,
                );
                pairs.push((key, value));
            }

            Filter::Object(pairs)
        }
        "number" => {
            let num = code[root.range().start_byte..root.range().end_byte]
                .parse::<f64>()
                .expect("number should be a valid float");
            Filter::Number(num)
        }
        "string" => {
            let s = code[root.range().start_byte + 1..root.range().end_byte - 1].to_string();
            Filter::String(s)
        }
        "pipeline" => {
            let lhs = parse_filter(code, root.child(0).expect("pipe should have a lhs"), defs);
            let rhs = parse_filter(code, root.child(2).expect("pipe should have a rhs"), defs);
            Filter::Pipe(Box::new(lhs), Box::new(rhs))
        }
        "binary_expression" => {
            let lhs = parse_filter(
                code,
                root.child(0).expect("binary expression should have a lhs"),
                defs,
            );
            let rhs = parse_filter(
                code,
                root.child(2).expect("binary expression should have a rhs"),
                defs,
            );
            let op = match &code
                [root.child(1).unwrap().range().start_byte..root.child(1).unwrap().range().end_byte]
            {
                "+" => crate::filter::BinOp::Add,
                "-" => crate::filter::BinOp::Sub,
                "*" => crate::filter::BinOp::Mul,
                "/" => crate::filter::BinOp::Div,
                "%" => crate::filter::BinOp::Mod,
                ">" => crate::filter::BinOp::Gt,
                "<" => crate::filter::BinOp::Lt,
                ">=" => crate::filter::BinOp::Ge,
                "<=" => crate::filter::BinOp::Le,
                "==" => crate::filter::BinOp::Eq,
                "!=" => crate::filter::BinOp::Ne,
                "and" => crate::filter::BinOp::And,
                "or" => crate::filter::BinOp::Or,
                _ => panic!("unknown binary operator"),
            };
            Filter::BinOp(Box::new(lhs), op, Box::new(rhs))
        }
        "unary_expression" => {
            let rhs = parse_filter(
                code,
                root.child(1).expect("unary expression should have a rhs"),
                defs,
            );
            let op = match &code
                [root.child(0).unwrap().range().start_byte..root.child(0).unwrap().range().end_byte]
            {
                "-" => crate::filter::UnOp::Neg,
                "not" => crate::filter::UnOp::Not,
                _ => panic!("unknown unary operator"),
            };
            Filter::UnOp(op, Box::new(rhs))
        }
        "true" => Filter::Boolean(true),
        "false" => Filter::Boolean(false),
        "null" => Filter::Null,
        "parenthesized_expression" => parse_filter(
            code,
            root.child(1)
                .expect("paranthesized expression should have a value"),
            defs,
        ),
        "if_expression" => {
            let cond = parse_filter(
                code,
                root.child(1)
                    .expect("if expression should have a condition"),
                defs,
            );
            let then = parse_filter(
                code,
                root.child(3).expect("if expression should have a then"),
                defs,
            );
            let elifs: Vec<(Filter, Filter)> = (4..root.child_count() - 2)
                .map(|i| {
                    let elif = root.child(i).expect("if expression should have an elif");
                    let cond = parse_filter(
                        code,
                        elif.child(1)
                            .expect("elif expression should have a condition"),
                        defs,
                    );
                    let then = parse_filter(
                        code,
                        elif.child(3).expect("elif expression should have a then"),
                        defs,
                    );
                    (cond, then)
                })
                .collect();

            let else_ = if root.child_count() == 5 {
                Filter::Dot
            } else {
                parse_filter(
                    code,
                    root.child(root.child_count() - 2)
                        .expect("if expression should have an else"),
                    defs,
                )
            };

            Filter::IfThenElse(
                Box::new(cond),
                Box::new(then),
                Box::new(elifs.into_iter().rev().fold(else_, |acc, (cond, then)| {
                    Filter::IfThenElse(Box::new(cond), Box::new(then), Box::new(acc))
                })),
            )
        }
        "else_expression" => parse_filter(
            code,
            root.child(1).expect("else expression should have a value"),
            defs,
        ),
        "call_expression" => {
            let name = code[root.child(0).unwrap().range().start_byte
                ..root.child(0).unwrap().range().end_byte]
                .to_string();
            let args = root.child(1);
            if let Some(args) = args {
                let args: Vec<Filter> = (1..args.child_count())
                    .step_by(2)
                    .map(|i| {
                        parse_filter(
                            code,
                            args.child(i)
                                .expect("call expression should have arguments"),
                            defs,
                        )
                    })
                    .collect();
                Filter::Call(name, Some(args))
            } else {
                Filter::Call(name, None)
            }
        }
        "function_definition" => {
            let name = code[root.child(1).unwrap().range().start_byte
                ..root.child(1).unwrap().range().end_byte]
                .to_string();
            let args = root
                .child(2)
                .expect("function definition should have arguments");
            let args: Vec<String> = (1..args.child_count())
                .step_by(2)
                .map(|i| {
                    code[args.child(i).unwrap().range().start_byte
                        ..args.child(i).unwrap().range().end_byte]
                        .to_string()
                })
                .collect();
            let body = parse_filter(
                code,
                root.child(root.child_count() - 2)
                    .expect("function definition should have a body"),
                defs,
            );
            tracing::trace!(
                "Parsed function definition: {}({}) -> {}",
                name,
                args.join(", "),
                body
            );
            defs.insert(name.clone(), Filter::Bound(args, Box::new(body)));
            Filter::Dot
        }

        "function_expression" => {
            println!("Child count: {}", root.child_count());
            for i in 0..root.child_count() {
                let child = root.child(i).unwrap();
                let text = &code[child.range().start_byte..child.range().end_byte];
                println!("Child {}: {} = '{}'", i, child.kind(), text);
            }

            let mut final_expr = Filter::Dot;

            let mut inner_defs = HashMap::new();

            for i in 0..root.child_count() {
                let child = root.child(i).unwrap();

                match child.kind() {
                    "function_definition" => {
                        let _ = parse_filter(code, child, &mut inner_defs);
                    }
                    _ => {
                        final_expr = parse_filter(code, child, &mut inner_defs);
                    }
                }
            }

            let result = Filter::FunctionExpression(inner_defs, Box::new(final_expr));
            tracing::trace!("Parsed function expression:\n{}", result);
            result
        }
        "binding_expression" => {
            // .name as $nick e.g

            Filter::Dot
        }
        "optional_expression" => {
            println!("Child count: {}", root.child_count());
            for i in 0..root.child_count() {
                let child = root.child(i).unwrap();
                let text = &code[child.range().start_byte..child.range().end_byte];
                println!("Child {}: {} = '{}'", i, child.kind(), text);
            }
            let field = root
                .child(0)
                .expect("optional expression should have the first child as its field");

            let identifier = parse_filter(
                code,
                field
                    .child(1)
                    .expect("field access should have the second child as its field"),
                defs,
            );
            Filter::ObjIndex(identifier.to_string())
        }
        "reduce_expression"
        | "assignment_expression"
        | "field_expression"
        | "foreach_expression" => {
            tracing::warn!("todo: {} are not yet supported", root.kind());
            unimplemented!(
                "todo: '{}' are not yet supported, encountered '{}'",
                root.kind(),
                &code[root.range().start_byte..root.range().end_byte]
            );
        }
        _ => {
            tracing::warn!(
                "unknown filter {} {}",
                root.kind(),
                code[root.range().start_byte..root.range().end_byte].to_string()
            );
            Filter::Dot
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::filter::Filter;

    use super::*;

    #[test]
    fn test_parse() {
        let code = r#"
            def add(a; b): a + b;
            add(1; 2)
        "#;

        let (defs, filter) = parse(code);

        assert_eq!(
            defs.get("add").unwrap().clone(),
            Filter::Bound(
                vec!["a".to_string(), "b".to_string()],
                Box::new(Filter::BinOp(
                    Box::new(Filter::Call("a".to_string(), None)),
                    crate::filter::BinOp::Add,
                    Box::new(Filter::Call("b".to_string(), None))
                ))
            )
        );

        assert_eq!(
            filter,
            Filter::Call(
                "add".to_string(),
                Some(vec![Filter::Number(1.0), Filter::Number(2.0)])
            )
        );
    }

    #[test]
    fn test_parse_filter() {
        let code = r#"
            1 + 2
        "#;

        let (defs, filter) = parse(code);

        assert_eq!(defs, HashMap::new());

        assert_eq!(
            filter,
            Filter::BinOp(
                Box::new(Filter::Number(1.0)),
                crate::filter::BinOp::Add,
                Box::new(Filter::Number(2.0))
            )
        );
    }

    #[test]
    fn test_parse_function_definition() {
        let code = r#"
            (def asd:  .[] ;  .) | asd
        "#;
        let (defs, filter) = parse(code);
        assert_eq!(
            defs.get("main").unwrap().clone(),
            Filter::Bound(
                vec![],
                Box::new(Filter::FunctionExpression(
                    HashMap::from([
                        (
                            "add".to_string(),
                            Filter::Bound(
                                vec!["a".to_string(), "b".to_string()],
                                Box::new(Filter::BinOp(
                                    Box::new(Filter::Call("a".to_string(), None)),
                                    crate::filter::BinOp::Add,
                                    Box::new(Filter::Call("b".to_string(), None))
                                ))
                            )
                        ),
                        (
                            "sub".to_string(),
                            Filter::Bound(
                                vec!["a".to_string(), "b".to_string()],
                                Box::new(Filter::BinOp(
                                    Box::new(Filter::Call("a".to_string(), None)),
                                    crate::filter::BinOp::Sub,
                                    Box::new(Filter::Call("b".to_string(), None))
                                ))
                            )
                        ),
                        (
                            "mul".to_string(),
                            Filter::Bound(
                                vec!["a".to_string(), "b".to_string()],
                                Box::new(Filter::BinOp(
                                    Box::new(Filter::Call("a".to_string(), None)),
                                    crate::filter::BinOp::Mul,
                                    Box::new(Filter::Call("b".to_string(), None))
                                ))
                            )
                        ),
                    ]),
                    Box::new(Filter::Call(
                        "add".to_string(),
                        Some(vec![Filter::Number(1.0), Filter::Number(2.0)])
                    ))
                ))
            )
        );
        assert_eq!(
            filter,
            Filter::Call(
                "sub".to_string(),
                Some(vec![Filter::Number(3.0), Filter::Number(1.0)])
            )
        );
    }

    #[test]
    fn test_parse_variable() {
        let filter = EnvFilter::from_default_env();
        tracing_subscriber::fmt()
            .with_env_filter(filter)
            .with_thread_ids(true)
            .with_thread_names(true)
            .with_file(true)
            .with_line_number(true)
            .init();

        let code = r#"
            def add(a; b): a + b;
                def sub(a; b): a - b;
                def mul(a; b): a * b;
                add(1; 2);
            sub(3; 1)
        "#;
        let (defs, filter) = parse(code);
        assert_eq!(
            defs.get("main").unwrap().clone(),
            Filter::Bound(
                vec![],
                Box::new(Filter::FunctionExpression(
                    HashMap::from([
                        (
                            "add".to_string(),
                            Filter::Bound(
                                vec!["a".to_string(), "b".to_string()],
                                Box::new(Filter::BinOp(
                                    Box::new(Filter::Call("a".to_string(), None)),
                                    crate::filter::BinOp::Add,
                                    Box::new(Filter::Call("b".to_string(), None))
                                ))
                            )
                        ),
                        (
                            "sub".to_string(),
                            Filter::Bound(
                                vec!["a".to_string(), "b".to_string()],
                                Box::new(Filter::BinOp(
                                    Box::new(Filter::Call("a".to_string(), None)),
                                    crate::filter::BinOp::Sub,
                                    Box::new(Filter::Call("b".to_string(), None))
                                ))
                            )
                        ),
                        (
                            "mul".to_string(),
                            Filter::Bound(
                                vec!["a".to_string(), "b".to_string()],
                                Box::new(Filter::BinOp(
                                    Box::new(Filter::Call("a".to_string(), None)),
                                    crate::filter::BinOp::Mul,
                                    Box::new(Filter::Call("b".to_string(), None))
                                ))
                            )
                        ),
                    ]),
                    Box::new(Filter::Call(
                        "add".to_string(),
                        Some(vec![Filter::Number(1.0), Filter::Number(2.0)])
                    ))
                ))
            )
        );
        assert_eq!(
            filter,
            Filter::Call(
                "sub".to_string(),
                Some(vec![Filter::Number(3.0), Filter::Number(1.0)])
            )
        );
    }

}
