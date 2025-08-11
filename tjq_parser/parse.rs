use std::{collections::HashMap, vec};

use tree_sitter::Node;

use tjq_exec::{BinOp, Filter, UnOp};
use crate::printer::print_ast;

pub fn parse(code: &str) -> (HashMap<String, Filter>, Filter) {
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&tree_sitter_tjq::LANGUAGE.into())
        .expect("Error loading jq grammar");
    let tree = parser.parse(code, None).unwrap();

    // assert_eq!(tree.root_node().kind(), "program");

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
               println!("unexpected node in program: {}", child.kind());
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

pub fn parse_defs(code: &str) -> HashMap<String, Filter> {
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&tree_sitter_tjq::LANGUAGE.into())
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

fn parse_child_or_hole(
    code: &str,
    root: Node<'_>,
    index: usize,
    defs: &mut HashMap<String, Filter>,
) -> Filter {
    root.child(index)
        .map(|node| parse_filter(code, node, defs))
        .unwrap_or(Filter::Hole)
}

pub(crate) fn parse_filter(
    code: &str,
    root: Node<'_>,
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
                let rhs = code[rhs.range().start_byte..rhs.range().end_byte].parse::<isize>().unwrap_or_else(|_| panic!("array index should be a valid integer(expressions in the index are not yet supported), encountered '{}'", &code[rhs.range().start_byte..rhs.range().end_byte]));
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
            "" => Filter::Hole,
            s => Filter::Call(s.to_string(), None),
        },
        "variable" => {
            let name = &code[root.range().start_byte + 1..root.range().end_byte];
            Filter::Variable(name.to_string())
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
            // let lhs = parse_filter(code, root.child(0).expect("pipe should have a lhs"), defs);
            let lhs = parse_child_or_hole(code, root, 0, defs);
            // let rhs = parse_filter(code, root.child(2).expect("pipe should have a rhs"), defs);
            let rhs = parse_child_or_hole(code, root, 2, defs);
            
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
                "+" => BinOp::Add,
                "-" => BinOp::Sub,
                "*" => BinOp::Mul,
                "/" => BinOp::Div,
                "%" => BinOp::Mod,
                ">" => BinOp::Gt,
                "<" => BinOp::Lt,
                ">=" => BinOp::Ge,
                "<=" => BinOp::Le,
                "==" => BinOp::Eq,
                "!=" => BinOp::Ne,
                "and" => BinOp::And,
                "or" => BinOp::Or,
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
                "-" => UnOp::Neg,
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
        // "paranthesized_expression" => {
        //     let mut inner_defs = defs.clone();

        //     parse_filter(code,
        //          root.child(1)
        //                 .expect("paranthesized expression should have a value"),
        //          &mut inner_defs,

        // )
        // }
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
                let end = &code[root.range().start_byte + 1..root.range().end_byte];
                if end == "end" {
                    Filter::Dot
                } else {
                    Filter::Hole
                }
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
        "else_expression" => parse_child_or_hole(code, root,1,defs),
        
        
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
            // println!("Child count: {}", root.child_count());
            // for i in 0..root.child_count() {
            //     let child = root.child(i).unwrap();
            //     let text = &code[child.range().start_byte..child.range().end_byte];
            //     println!("Child {}: {} = '{}'", i, child.kind(), text);
            // }

            let lhs = parse_filter(code, root.child(0).unwrap(), defs);
            let pat = parse_filter(code, root.child(2).unwrap(), defs);
            Filter::BindingExpression(Box::new(lhs), Box::new(pat))
        }
        "optional_expression" => {
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
        "reduce_expression" => {
            // println!("Child count: {}", root.child_count());
            // for i in 0..root.child_count() {
            //     let child = root.child(i).unwrap();
            //     let text = &code[child.range().start_byte..child.range().end_byte];
            //     println!("Child {}: {} = '{}'", i, child.kind(), text);
            // }            let source_node = bind.child(0).unwrap();
            let bind = root.child(1).unwrap();

            if bind.range().start_byte == bind.range().end_byte {
                //if empty 
                todo!();
            }
            let source_node = bind.child(0).unwrap();

            let var_node = bind.child(2).unwrap();
            let source = parse_filter(code, source_node, defs);

            let var_name = &code[var_node.range().start_byte + 1..var_node.range().end_byte];
            let mut var_def = HashMap::new();
            var_def.insert(var_name.to_string(), source.clone());

            let init = parse_filter(code, root.child(3).unwrap(), defs);
            let update = parse_filter(code, root.child(5).unwrap(), defs);

            Filter::ReduceExpression(var_def, Box::new(init), Box::new(update))
        }
        "assignment_expression" => Filter::Dot,
        "foreach_expression" => Filter::Dot,
        "field_expression" => Filter::Dot,
         "hole" => Filter::Hole,
        "slice_expression" => todo!(),

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

    use std::process::CommandEnvs;

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
                    BinOp::Add,
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
                BinOp::Add,
                Box::new(Filter::Number(2.0))
            )
        );
    }

    #[test]
    fn test_parse_function_definition() {
        let code = r#"
            (def f:  .[] ;  .) | f
        "#;
        let (defs, filter) = parse(code);
        assert!(defs.is_empty());
        assert_eq!(
            filter,
            Filter::Pipe(
                Box::new(Filter::FunctionExpression(
                    HashMap::from([(
                        "f".to_string(),
                        Filter::Bound(vec![], Box::new(Filter::ArrayIterator))
                    )]),
                    Box::new(Filter::Dot)
                )),
                Box::new(Filter::Call("f".to_string(), None))
            )
        );
    }

    #[test]
    fn test_parse_variable() {
        let code = r#"
            def main:
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
                                    BinOp::Add,
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
                                    BinOp::Sub,
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
                                    BinOp::Mul,
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
    fn test_if_expression(){
        let code = r#"
            if true then
                1
            else
                2
                end
        "#;

        let (defs, filter) = parse(code);

        assert!(defs.is_empty());
        assert_eq!(
            filter,
            Filter::IfThenElse(
                Box::new(Filter::Boolean(true)),
                Box::new(Filter::Number(1.0)),
                Box::new(Filter::Number(2.0))
            )
        );
    }

    #[test]
    fn test_binding_expression(){
        let code = r#"
           . as $item 
        "#;
        let (defs, filter) = parse(code);

        assert_eq!(
            filter,
            Filter::BindingExpression(
                Box::new(Filter::Dot),
                Box::new(Filter::Variable("item".to_string()))
            )
        );
    
    
        }

    #[test]
    fn test_binding_expression2(){
        let code = r#"
           1 as $item  | $item
        "#;
        let (defs, filter) = parse(code);

        assert_eq!(
            filter,
            Filter::Pipe((Box::new(Filter::BindingExpression(
                Box::new(Filter::Number(1.0)),
                Box::new(Filter::Variable("item".to_string()))
            ))), 
                Box::new(Filter::Variable("item".to_string()),
            ))
            
        );    
    
        }

    #[test]
    fn test_binding_expression3(){
        let code = r#"
           [1,2,3] as $item  | $item
        "#;
        let (defs, filter) = parse(code);

        assert_eq!(
            filter,
            Filter::Pipe((Box::new(Filter::BindingExpression(
                Box::new(Filter::Array(vec![Filter::Number(1.0), Filter::Number(2.0), Filter::Number(3.0)]))  ,
                Box::new(Filter::Variable("item".to_string()))
            ))), 
                Box::new(Filter::Variable("item".to_string()),
            ))
            
        );    
    
        }


    #[test]
    fn test_incomplete_binop() {
        let code = r#"1 + "#;

        let (defs, filter) = parse(code);

        assert!(defs.is_empty());
        assert_eq!(
            filter,
            Filter::BinOp(
                Box::new(Filter::Number(1.0)),
                BinOp::Add,
                Box::new(Filter::Hole)
            )
        );
    }

    #[test]
    fn test_incomplete_binop2 (){
        let code = r#"1 == "#;

        let (defs, filter) = parse(code);

        assert!(defs.is_empty());
        assert_eq!(
            filter,
            Filter::BinOp(
                Box::new(Filter::Number(1.0)),
                BinOp::Eq,
                Box::new(Filter::Hole)
            )
        );
    }


    #[test]
    fn test_incomplete_pipe() {
        let code = r#"1 | "#;

        let (defs, filter) = parse(code);

        assert!(defs.is_empty());
        assert_eq!(
            filter,
            Filter::Pipe(Box::new(Filter::Number(1.0)), Box::new(Filter::Hole))
        );
    }
     #[test]
    fn test_incomplete_pipe2() {
        let code = r#" | 1 "#;

        let (defs, filter) = parse(code);

        assert!(defs.is_empty());
        assert_eq!(
            filter,
            Filter::Pipe(Box::new(Filter::Hole), Box::new(Filter::Number(1.0)))
        );
    }

    #[test]
    fn test_incomplete_comma() {
        let code = r#"1, "#;

        let (defs, filter) = parse(code);

        assert!(defs.is_empty());
        assert_eq!(
            filter,
            Filter::Comma(Box::new(Filter::Number(1.0)), Box::new(Filter::Hole))
        );
    }

    #[test]
    fn test_incomplete_function_definition() {
        let code = r#"
            def f(a; b):
        "#;

        let (defs, filter) = parse(code);

        assert_eq!(
            defs.get("f"),
            Some(&Filter::Bound(
                vec!["a".to_string(), "b".to_string()],
                Box::new(Filter::Hole)
            ))
        );
        assert_eq!(filter, Filter::Hole);
    }

    #[test]
    fn test_incomplete_function_definition2() {
        let code = r#"
            def f(a; b):
                a*b
        "#;

        let (defs, filter) = parse(code);

        assert_eq!(
            defs.get("f"),
            Some(&Filter::Bound(
                vec!["a".to_string(), "b".to_string()],
                Box::new(
                    Filter::BinOp(
                        Box::new(Filter::Call("a".to_string(), None)),
                        BinOp::Mul,
                        Box::new(Filter::Call("b".to_string(), None))
                    )
                )
            ))
        );
        // assert_eq!(filter, Filter::Hole);
        //check this!
    }

    #[test]
    fn test_incomplete_function_expression() {
        let code = r#"
            (def f(a; b): a + b; )
        "#;

        let (defs, filter) = parse(code);

        assert!(defs.is_empty());
        assert_eq!(
            filter,
            Filter::FunctionExpression(
                HashMap::from([(
                    "f".to_string(),
                    Filter::Bound(
                        vec!["a".to_string(), "b".to_string()],
                        Box::new(Filter::BinOp(
                            Box::new(Filter::Call("a".to_string(), None)),
                            BinOp::Add,
                            Box::new(Filter::Call("b".to_string(), None))
                        ))
                    )
                )]),
                Box::new(Filter::Hole)
            )
        );
    }

    #[test]
    fn test_incomplete_nested_function_expression() {
        let code = r#"
            (def f(a; b): 
                def g(c; d):
            )

        "#;

        let (defs, filter) = parse(code);

        assert!(defs.is_empty());
        assert_eq!(
            filter,
            Filter::FunctionExpression(
                HashMap::from([(
                    "f".to_string(),
                    Filter::Bound(
                        vec!["a".to_string(), "b".to_string()],
                        Box::new(Filter::FunctionExpression(
                            HashMap::from([(
                                "g".to_string(),
                                Filter::Bound(
                                    vec!["c".to_string(), "d".to_string()],
                                    Box::new(Filter::Hole)
                                )
                            )]),
                            Box::new(Filter::Hole)
                        ))
                    )
                )]),
                Box::new(Filter::Hole)
            )
        );
    }

    #[test]
    fn test_incomplete_nested_function_expression2() {
        let code = r#"
            (def f(a; b): 
                def g(c; d):
                    c + d;
            )

        "#;

        let (defs, filter) = parse(code);

        assert!(defs.is_empty());
        assert_eq!(
            filter,
            Filter::FunctionExpression(
                HashMap::from([(
                    "f".to_string(),
                    Filter::Bound(
                        vec!["a".to_string(), "b".to_string()],
                        Box::new(Filter::FunctionExpression(
                            HashMap::from([(
                                "g".to_string(),
                                Filter::Bound(
                                    vec!["c".to_string(), "d".to_string()],
                                    Box::new(Filter::BinOp(
                                        Box::new(Filter::Call("c".to_string(), None)),
                                        BinOp::Add,
                                        Box::new(Filter::Call("d".to_string(), None))
                                    ))
                                )
                            )]),
                            Box::new(Filter::Hole)
                        ))
                    )
                )]),
                Box::new(Filter::Hole)
            )
        );
    }

    #[test]
    fn test_incomplete_nested_function_expression3() {
        let code = r#"
            (def f(a; b): 
                def g(c; d):
                    c + d;
                e + f;
            )

        "#;

        let (defs, filter) = parse(code);

        assert!(defs.is_empty());
        assert_eq!(
            filter,
            Filter::FunctionExpression(
                HashMap::from([(
                    "f".to_string(),
                    Filter::Bound(
                        vec!["a".to_string(), "b".to_string()],
                        Box::new(Filter::FunctionExpression(
                            HashMap::from([(
                                "g".to_string(),
                                Filter::Bound(
                                    vec!["c".to_string(), "d".to_string()],
                                    Box::new(Filter::BinOp(
                                        Box::new(Filter::Call("c".to_string(), None)),
                                        BinOp::Add,
                                        Box::new(Filter::Call("d".to_string(), None))
                                    ))
                                )
                            )]),
                            Box::new(Filter::BinOp(
                                Box::new(Filter::Call("e".to_string(), None)),
                                BinOp::Add,
                                Box::new(Filter::Call("f".to_string(), None))
                            ))
                        ))
                    )
                )]),
                Box::new(Filter::Hole)
            )
        );
    }

    #[test]
    fn test_incomplete_if_then_else() {
        let code = r#"
            if true then
                1
            else  end
            
        "#;

        let (defs, filter) = parse(code);

        assert!(defs.is_empty());
        assert_eq!(
            filter,
            Filter::IfThenElse(
                Box::new(Filter::Boolean(true)),
                Box::new(Filter::Number(1.0)),
                Box::new(Filter::Hole)
            )
        );
    }

    #[test]
    fn test_incomplete_if_then_else2() {
        let code = r#"
            if true then
                1
        "#;

        let (defs, filter) = parse(code);

        assert!(defs.is_empty());
        assert_eq!(
            filter,
            Filter::IfThenElse(
                Box::new(Filter::Boolean(true)),
                Box::new(Filter::Number(1.0)),
                Box::new(Filter::Hole)
            )
        );
    }

    #[test]
    fn test_incomplete_if_then_else3() {
        let code = r#"
            if true then end
        "#;

        let (defs, filter) = parse(code);

        assert!(defs.is_empty());
        assert_eq!(
            filter,
            Filter::IfThenElse(
                Box::new(Filter::Boolean(true)),
                Box::new(Filter::Hole),
                Box::new(Filter::Hole)
            )
        );
    }

    #[test]
    fn test_incomplete_if_then_else4() {
        let code = r#"
            if true 
        "#;

        let (defs, filter) = parse(code);

        assert!(defs.is_empty());
        assert_eq!(
            filter,
            Filter::IfThenElse(
                Box::new(Filter::Boolean(true)),
                Box::new(Filter::Hole),
                Box::new(Filter::Hole)
            )
        );
    }

    #[test]
    fn test_incomplete_if_then_else5() {
        let code = r#"
            if
        "#;

        let (defs, filter) = parse(code);

        assert!(defs.is_empty());
        assert_eq!(
            filter,
            Filter::IfThenElse(
                Box::new(Filter::Hole),
                Box::new(Filter::Hole),
                Box::new(Filter::Hole)
            )
        );
    }

    #[test]
    fn test_hole_token(){
        let code = r#" . |  "#;

        let(defs,filter) = parse(code);
        assert!(defs.is_empty());
        assert_eq!(
            filter,
            Filter::Pipe(Box::new(Filter::Dot), Box::new(Filter::Hole) )   

        );
    }

    #[test]

    fn test_incomplete_unop(){
        
    }

    #[test]
    fn test_incomplete_reduce(){
        let code = r#"
            reduce .[] as $item 
        "#;

        let (defs, filter) = parse(code);

        assert!(defs.is_empty());
        assert_eq!(
            filter,
            Filter::ReduceExpression(
                HashMap::from([("item".to_string(), Filter::ArrayIterator)]),
                Box::new(Filter::Number(0.0)),
                Box::new(Filter::Hole)
            )
        );
    }

    #[test]
    fn test_incomplete_reduce2(){
        let code = r#"
            reduce  (;  . + 1 )
        "#;

        let (defs, filter) = parse(code);

        assert!(defs.is_empty());
        assert_eq!(
            filter,
            Filter::ReduceExpression(
                HashMap::new(),
                Box::new(Filter::Hole),
                Box::new(Filter::BinOp(
                    Box::new(Filter::Dot),
                    BinOp::Add,
                    Box::new(Filter::Number(1.0))
                ))
            )
        );
    }

    #[test]
    fn test_incomplete_reduce3(){
        let code = r#"
            reduce .[] as $item (  )
            
                    "#;

        let (defs, filter) = parse(code);
        //TODO check this (incomplete?)
        assert!(defs.is_empty());
        assert_eq!(
            filter,
           Filter::Dot
        );
    }
        
        
    


    #[test]

    fn test_incomplete_binding(){
        let code = r#"
            . as 
        "#;

        let (defs, filter) = parse(code);

        assert!(defs.is_empty());
        assert_eq!(
            filter,
            Filter::BindingExpression(
                Box::new(Filter::Dot),
                Box::new(Filter::Hole)
            )
        );
    }

    #[test]

    fn test_incomplete_binding2(){
        let code = r#"
            as $item 
        "#;

        let (defs, filter) = parse(code);

        assert!(defs.is_empty());
        assert_eq!(
            filter,
            Filter::BindingExpression(
                Box::new(Filter::Hole),
                Box::new(Filter::Hole)
            )
        );
    }

    #[test]
    fn test_incomplete_array(){

        let code = r#"
            [1,2,3,4
        "#;

        let (defs, filter) = parse(code);

        assert!(defs.is_empty());
        assert_eq!(
            filter,
            Filter::Array(vec![
                Filter::Number(1.0),
                Filter::Number(2.0),
                Filter::Number(3.0),
                Filter::Number(4.0),
            ])
        );
    }

    #[test]
    fn test_call_expression(){
        let code = r#"
            add(1; 2)
        "#;

        let (defs, filter) = parse(code);

        assert!(defs.is_empty());
        assert_eq!(
            filter,
            Filter::Call(
                "add".to_string(),
                Some(vec![Filter::Number(1.0), Filter::Number(2.0)])
            )
        );
    }

    #[test]
    fn test_incomplete_call_expression() {
        let code = r#"
            add(1; )
        "#;

        let (defs, filter) = parse(code);

        assert!(defs.is_empty());
        assert_eq!(
            filter,
            Filter::Call(
                "add".to_string(),
                Some(vec![Filter::Number(1.0), Filter::Hole])
            )
        );
    }
    #[test]
    fn test_incomplete_call_expression2() {
        let code = r#"
            add(; 1)
        "#;

        let (defs, filter) = parse(code);

        assert!(defs.is_empty());
        assert_eq!(
            filter,
            Filter::Call(
                "add".to_string(),
                Some(vec![Filter::Hole, Filter::Number(1.0)])
            )
        );
    }

    #[test]
    fn test_subscript_expression(){

        let code = r#"
           . | .[2]
        "#;

        let (defs, filter) = parse(code);
        assert!(defs.is_empty());
        assert_eq!(
            filter,
            Filter::Pipe(
                Box::new(Filter::Dot),
                Box::new(Filter::ArrayIndex(2))
            )
        );
    }
    // #[test]
    // fn test_array_slice() {
    //     let code = r#"
    //         .[1:3]
    //     "#;

    //     let (defs, filter) = parse(code);
    //     assert!(defs.is_empty());
    //     assert_eq!(
    //         filter,
    //         Filter::SliceExpression(1, 3)
    //     );
    // }
    



    
    

}
