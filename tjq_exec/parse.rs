use std::fmt::{Display, Formatter};
use std::{collections::HashMap, vec};

use tree_sitter::{Node, Range};

use crate::printer::print_ast;
use crate::{BinOp, Filter, UnOp};

pub(crate) struct Cst<'a> {
    pub range: Range,
    pub value: &'a str,
    pub kind: FilterKind,
    pub children: Vec<Cst<'a>>,
}

pub(crate) enum FilterKind {
    Dot,
    Pipe,
    Comma,
    ObjIndex,
    ArrayIndex,
    ArrayIterator,
    Null,
    Boolean,
    Number,
    String,
    Array,
    Object,
    BinOp(BinOp),
    UnOp(UnOp),
    Variable,
    FunctionExpression,
    Call,
    IfThenElse,
    Bound,
    ReduceExpression,
    Hole,
}

impl From<&Cst<'_>> for Filter {
    fn from(cst: &Cst<'_>) -> Filter {
        match cst.kind {
            FilterKind::Hole => {
                assert!(cst.children.len() == 0);
                Filter::Hole
            }
            FilterKind::Dot => {
                assert!(cst.children.len() == 0);
                Filter::Dot
            }
            FilterKind::Pipe => {
                assert!(cst.children.len() == 2);
                Filter::Pipe(
                    Box::new((&cst.children[0]).into()),
                    Box::new((&cst.children[1]).into()),
                )
            }
            FilterKind::Comma => {
                assert!(cst.children.len() == 2);
                Filter::Comma(
                    Box::new((&cst.children[0]).into()),
                    Box::new((&cst.children[1]).into()),
                )
            }
            FilterKind::ObjIndex => {
                assert!(cst.children.len() == 1);
                Filter::ObjIndex(Box::new((&cst.children[0]).into()))
            }
            FilterKind::ArrayIndex => {
                assert!(cst.children.len() == 1);
                Filter::ArrayIndex(Box::new((&cst.children[0]).into()))
            }
            FilterKind::ArrayIterator => {
                assert!(cst.children.len() == 0);
                Filter::ArrayIterator
            }
            FilterKind::Null => {
                assert!(cst.children.len() == 0);
                Filter::Null
            }
            FilterKind::Boolean => {
                assert!(cst.children.len() == 1);
                assert!(cst.children[0].value == "true" || cst.children[0].value == "false");
                Filter::Boolean(cst.children[0].value == "true")
            }
            FilterKind::Number => {
                assert!(cst.children.len() == 1);
                Filter::Number(cst.children[0].value.parse().unwrap())
            }
            FilterKind::String => {
                assert!(cst.children.len() == 1);
                Filter::String(cst.children[0].value.to_string())
            }
            FilterKind::Array => Filter::Array(cst.children.iter().map(|c| c.into()).collect()),
            FilterKind::Object => Filter::Object(
                cst.children
                    .iter()
                    .map(|c| {
                        assert!(c.children.len() == 2);
                        ((&c.children[0]).into(), (&c.children[1]).into())
                    })
                    .collect(),
            ),
            FilterKind::BinOp(binop) => {
                assert!(cst.children.len() == 3);
                Filter::BinOp(
                    Box::new((&cst.children[0]).into()),
                    binop,
                    Box::new((&cst.children[2]).into()),
                )
            }
            FilterKind::UnOp(unop) => {
                assert!(cst.children.len() == 2);
                Filter::UnOp(unop, Box::new((&cst.children[1]).into()))
            }
            FilterKind::Variable => {
                assert!(cst.children.len() == 1);
                Filter::Variable(cst.children[0].value.to_string())
            }
            FilterKind::FunctionExpression => {
                assert!(cst.children.len() >= 1);
                todo!()
            }
            FilterKind::Call => todo!(),
            FilterKind::IfThenElse => {
                assert!(cst.children.len() == 3);
                Filter::IfThenElse(
                    Box::new((&cst.children[0]).into()),
                    Box::new((&cst.children[1]).into()),
                    Box::new((&cst.children[2]).into()),
                )
            }
            FilterKind::Bound => todo!(),
            FilterKind::ReduceExpression => todo!(),
        }
    }
}

impl Display for FilterKind {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            FilterKind::Dot => write!(f, "."),
            FilterKind::Hole => write!(f, "??"),
            FilterKind::ArrayIndex => write!(f, "[]"),
            FilterKind::Object => write!(f, "{{}}"),
            FilterKind::FunctionExpression => write!(f, "function"),
            FilterKind::Call => write!(f, "call"),
            FilterKind::IfThenElse => write!(f, "if"),
            FilterKind::Bound => write!(f, "bound"),
            FilterKind::ReduceExpression => write!(f, "reduce"),
            FilterKind::Pipe => todo!(),
            FilterKind::Comma => todo!(),
            FilterKind::ObjIndex => todo!(),
            FilterKind::ArrayIterator => todo!(),
            FilterKind::Null => todo!(),
            FilterKind::Boolean => todo!(),
            FilterKind::Number => todo!(),
            FilterKind::String => todo!(),
            FilterKind::Array => todo!(),
            FilterKind::BinOp(bin_op) => todo!(),
            FilterKind::UnOp(un_op) => todo!(),
            FilterKind::Variable => todo!(),
        }
    }
}

impl Display for Cst<'_> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "Cst({})", self.value)?;
        if !self.children.is_empty() {
            write!(f, " -> [")?;
            for child in &self.children {
                write!(f, "{}, ", child)?;
            }
            write!(f, "]")?;
        }
        Ok(())
    }
}

impl<'a> Cst<'a> {
    pub(crate) fn dot(range: Range) -> Self {
        Self {
            kind: FilterKind::Dot,
            children: Vec::new(),
            range,
            value: ".",
        }
    }

    pub(crate) fn hole(range: Range) -> Self {
        Self {
            kind: FilterKind::Hole,
            range,
            children: Vec::new(),
            value: "??",
        }
    }
    pub(crate) fn pipe(lhs: Cst<'a>, rhs: Cst<'a>, value: &'a str, range: Range) -> Self {
        Self {
            kind: FilterKind::Pipe,
            children: vec![lhs, rhs],
            range,
            value,
        }
    }
}

pub fn parse(code: &str) -> (HashMap<String, Cst>, Cst) {
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
            "comment" => {
                tracing::trace!(
                    "Skipping comment: {}",
                    &code[child.range().start_byte..child.range().end_byte]
                );
            }
            _ => {
                panic!("unexpected node in program: {}", child.kind());
            }
        }
    }

    defs
}

fn parse_child_or_hole<'a>(
    code: &'a str,
    root: Node<'a>,
    index: usize,
    defs: &mut HashMap<String, Cst<'a>>,
) -> Cst<'a> {
    root.child(index)
        .map(|node| parse_filter(code, node, defs))
        .unwrap_or(Cst::hole(root.range()))
}

pub(crate) fn parse_filter<'a>(
    code: &'a str,
    root: Node<'a>,
    defs: &'a mut HashMap<String, Cst<'_>>,
) -> Cst<'a> {
    tracing::trace!(
        "{}: {}",
        root.kind(),
        &code[root.range().start_byte..root.range().end_byte]
    );

    match root.kind() {
        "dot" => Cst::dot(root.range()),
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
            Cst::pipe(
                lhs,
                rhs,
                &code[root.range().start_byte..root.range().end_byte],
                root.range(),
            )
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
                // Filter::ArrayIndex(rhs)
                todo!("@can: parse the rhs as a Filter")
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
            // Filter::ObjIndex(identifier.to_string())
            todo!("@can: parse them as object indexes")
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

            let tail = root
                .child(root.child_count() - 2)
                .expect("if expression should end with 'end' or 'else <expr> end'");
            let tail_str = &code[tail.range().start_byte..tail.range().end_byte];

            let else_ = if tail_str == "end" {
                Filter::Dot
            } else if tail_str != "elseX" {
                Filter::Hole
            } else {
                // if … then … else <expr> end
                println!("{}", tail_str);
                parse_filter(code, tail, defs)
            };

            Filter::IfThenElse(
                Box::new(cond),
                Box::new(then),
                Box::new(elifs.into_iter().rev().fold(else_, |acc, (cond, then)| {
                    Filter::IfThenElse(Box::new(cond), Box::new(then), Box::new(acc))
                })),
            )
        }
        "else_expression" => parse_child_or_hole(code, root, 1, defs),

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
            // Filter::ObjIndex(identifier.to_string())
            todo!("@can: parse them as object indexes")
        }
        "reduce_expression" => {
            let bind = root.child(1).expect("reduce: expected binding_expression ");

            if bind.is_missing() || bind.range().start_byte == bind.range().end_byte {
                panic!("reduce: missing binding_expression ");
            }

            let source_node = bind
                .child(0)
                .expect("reduce: binding_expression missing source expression");
            let var_node = bind
                .child(2)
                .expect("reduce: binding_expression missing $variable");

            let generator = parse_filter(code, source_node, defs);

            let vtxt = &code[var_node.range().start_byte..var_node.range().end_byte];
            let var_name = vtxt
                .strip_prefix('$')
                .unwrap_or_else(|| panic!("reduce: expected variable"))
                .to_string();

            let init_node = root
                .child(3)
                .expect("reduce: missing initializer expression");
            let upd_node = root.child(5).expect("reduce: missing update expression");

            let init = parse_filter(code, init_node, defs);
            let update = parse_filter(code, upd_node, defs);

            Filter::ReduceExpression(
                var_name,
                Box::new(generator),
                Box::new(init),
                Box::new(update),
            )
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
            // panic!(
            //     "unknown filter {} {}",
            //     root.kind(),
            //     code[root.range().start_byte..root.range().end_byte].to_string()
            // );
            todo!("handle erroneous parse tree!")
        }
    }
}

#[cfg(test)]
mod tests {

    use std::process::CommandEnvs;

    use tracing_subscriber::EnvFilter;

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
    fn test_if_expression() {
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
    fn test_binding_expression() {
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
    fn test_binding_expression2() {
        let code = r#"
           1 as $item  | $item
        "#;
        let (defs, filter) = parse(code);

        assert_eq!(
            filter,
            Filter::Pipe(
                (Box::new(Filter::BindingExpression(
                    Box::new(Filter::Number(1.0)),
                    Box::new(Filter::Variable("item".to_string()))
                ))),
                Box::new(Filter::Variable("item".to_string()),)
            )
        );
    }

    #[test]
    fn test_binding_expression3() {
        let code = r#"
           [1,2,3] as $item  | $item
        "#;
        let (defs, filter) = parse(code);

        assert_eq!(
            filter,
            Filter::Pipe(
                (Box::new(Filter::BindingExpression(
                    Box::new(Filter::Array(vec![
                        Filter::Number(1.0),
                        Filter::Number(2.0),
                        Filter::Number(3.0)
                    ])),
                    Box::new(Filter::Variable("item".to_string()))
                ))),
                Box::new(Filter::Variable("item".to_string()),)
            )
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
    fn test_incomplete_binop2() {
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
        let _ = tracing_subscriber::fmt()
            .with_target(false)
            .with_thread_ids(false)
            .with_thread_names(false)
            .with_file(true)
            .with_line_number(true)
            .with_level(true)
            .without_time()
            .with_env_filter(EnvFilter::from_default_env())
            .try_init();
        let code = r#"1 | "#;
        let inferred = r#"1 | ??"#;

        let (_, filter) = parse(code);
        let (_, expected) = parse(inferred);
        assert_eq!(filter, expected);
    }

    #[test]
    fn test_incomplete_pipe2() {
        let code = r#" ?? | 1 "#;

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
    fn test_abs() {
        let (_, filter) = parse("if . < 0 then - . end");
        assert_eq!(
            filter,
            Filter::IfThenElse(
                Box::new(Filter::BinOp(
                    Box::new(Filter::Dot),
                    BinOp::Lt,
                    Box::new(Filter::Number(0.0))
                )),
                Box::new(Filter::UnOp(UnOp::Neg, Box::new(Filter::Dot))),
                Box::new(Filter::Dot)
            )
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
                Box::new(Filter::BinOp(
                    Box::new(Filter::Call("a".to_string(), None)),
                    BinOp::Mul,
                    Box::new(Filter::Call("b".to_string(), None))
                ))
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
            else  ?? end
            
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
    fn test_hole_token() {
        let code = r#" . | ?? "#;

        let (defs, filter) = parse(code);
        assert!(defs.is_empty());
        assert_eq!(
            filter,
            Filter::Pipe(Box::new(Filter::Dot), Box::new(Filter::Hole))
        );
    }
    #[test]
    fn test_reduce_expression() {
        let code = r#"
            reduce .[] as $item ( 0; . + 1 )
        "#;

        let (defs, filter) = parse(code);

        assert!(defs.is_empty());
        assert_eq!(
            filter,
            Filter::ReduceExpression(
                "item".to_string(),
                Box::new(Filter::ArrayIterator),
                Box::new(Filter::Number(0.0)),
                Box::new(Filter::BinOp(
                    Box::new(Filter::Dot),
                    BinOp::Add,
                    Box::new(Filter::Number(1.0))
                ))
            )
        );
    }
    #[test]

    fn test_incomplete_unop() {}

    #[test]
    fn test_incomplete_reduce() {
        let code = r#"
            reduce .[] as $item 
        "#;

        let (defs, filter) = parse(code);

        assert!(defs.is_empty());
        assert_eq!(
            filter,
            Filter::ReduceExpression(
                "item".to_string(),
                Box::new(Filter::Number(0.0)),
                Box::new(Filter::Hole),
                Box::new(Filter::Hole)
            )
        );
    }

    #[test]
    fn test_incomplete_reduce2() {
        let code = r#"
            reduce  (;  . + 1 )
        "#;

        let (defs, filter) = parse(code);

        assert!(defs.is_empty());
        assert_eq!(
            filter,
            Filter::ReduceExpression(
                "".to_string(),
                Box::new(Filter::Hole),
                Box::new(Filter::Hole),
                Box::new(Filter::BinOp(
                    Box::new(Filter::Dot),
                    BinOp::Add,
                    Box::new(Filter::Number(1.0))
                )),
            )
        );
    }

    #[test]
    fn test_incomplete_reduce3() {
        let code = r#"reduce .[] as $item (  )"#;
        let expected = r#"reduce .[] as $item (??; ??)"#;

        let (_, filter) = parse(code);
        let (_, expected) = parse(expected);
        //TODO check this (incomplete?)
        assert_eq!(filter, expected);
    }

    #[test]

    fn test_incomplete_binding() {
        let code = r#"
            . as 
        "#;

        let (defs, filter) = parse(code);

        assert!(defs.is_empty());
        assert_eq!(
            filter,
            Filter::BindingExpression(Box::new(Filter::Dot), Box::new(Filter::Hole))
        );
    }

    #[test]

    fn test_incomplete_binding2() {
        let code = r#"
            as $item 
        "#;

        let (defs, filter) = parse(code);

        assert!(defs.is_empty());
        assert_eq!(
            filter,
            Filter::BindingExpression(Box::new(Filter::Hole), Box::new(Filter::Hole))
        );
    }

    #[test]
    fn test_incomplete_array() {
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
    fn test_call_expression() {
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
    fn test_subscript_expression() {
        let code = r#"
           . | .[2]
        "#;

        let (defs, filter) = parse(code);
        assert!(defs.is_empty());
        assert_eq!(
            filter,
            Filter::Pipe(Box::new(Filter::Dot), Box::new(Filter::ArrayIndex(2)))
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
    #[test]
    fn test_incomplete_unary_expression() {
        let code = r#"-"#;

        let (defs, filter) = parse(code);

        assert!(defs.is_empty());
        assert_eq!(filter, Filter::UnOp(UnOp::Neg, Box::new(Filter::Hole)));
    }

    #[test]
    fn test_incomplete_object() {
        let code = r#"{"name": "john", "age":"#;

        let (defs, filter) = parse(code);

        assert!(defs.is_empty());
        assert_eq!(
            filter,
            Filter::Object(vec![
                (
                    Filter::String("name".to_string()),
                    Filter::String("john".to_string())
                ),
                (Filter::String("age".to_string()), Filter::Hole)
            ])
        );
    }

    #[test]
    fn test_incomplete_object_key() {
        let code = r#"{: "value"}"#;

        let (defs, filter) = parse(code);

        assert!(defs.is_empty());
        assert_eq!(
            filter,
            Filter::Object(vec![(Filter::Hole, Filter::String("value".to_string()))])
        );
    }

    #[test]
    fn test_incomplete_parenthesized() {
        let code = r#"("#;

        let (defs, filter) = parse(code);

        assert!(defs.is_empty());
        assert_eq!(filter, Filter::Hole);
    }

    #[test]
    fn test_incomplete_parenthesized_with_content() {
        let code = r#"(1 + "#;

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
    fn test_incomplete_subscript() {
        let code = r#".[1"#;

        let (defs, filter) = parse(code);
        //TODO : determine the output
        assert!(defs.is_empty());
        // This should handle incomplete array access
        assert_eq!(filter, Filter::ArrayIndex(1));
    }

    // #[test]
    // fn test_incomplete_try_catch() {
    //     let code = r#"try"#;

    //     let (defs, filter) = parse(code);

    //     assert!(defs.is_empty());
    //     // This should parse as a try expression with missing body
    //     assert_eq!(
    //         filter,
    //         Filter::TryExpression(Box::new(Filter::Hole), None)
    //     );
    // }

    // #[test]
    // fn test_incomplete_try2() {
    //     let code = r#"try .foo catch"#;

    //     let (defs, filter) = parse(code);

    //     assert!(defs.is_empty());
    //     assert_eq!(
    //         filter,
    //         Filter::TryExpression(
    //             Box::new(Filter::ObjIndex("foo".to_string())),
    //             Some(Box::new(Filter::Hole))
    //         )
    //     );
    // }

    #[test]
    fn test_multiple_incomplete_pipes() {
        let code = r#". | | ."#;

        let (defs, filter) = parse(code);

        assert!(defs.is_empty());
        assert_eq!(
            filter,
            Filter::Pipe(
                Box::new(Filter::Dot),
                Box::new(Filter::Pipe(Box::new(Filter::Hole), Box::new(Filter::Dot)))
            )
        );
    }

    #[test]
    fn test_incomplete_sequence() {
        let code = r#"1, 2,"#;
        let expected = r#"1, 2, ??"#;
        let (_, filter) = parse(code);
        let (_, expected) = parse(expected);
        //TODO check this (incomplete?)
        assert_eq!(filter, expected);
    }

    #[test]
    fn test_incomplete_nested_arrays() {
        let code = r#"[[1, 2], [3,"#;

        let (defs, filter) = parse(code);

        assert!(defs.is_empty());
        assert_eq!(
            filter,
            Filter::Array(vec![
                Filter::Array(vec![Filter::Number(1.0), Filter::Number(2.0)]),
                Filter::Array(vec![Filter::Number(3.0), Filter::Hole])
            ])
        );
    }

    #[test]
    fn test_incomplete_variable_reference() {
        let code = r#"$"#;

        let (defs, filter) = parse(code);

        assert!(defs.is_empty());
        assert_eq!(filter, Filter::Variable("".to_string()));
    }

    // #[test]
    // fn test_incomplete_assignment() {
    //     let code = r#".foo ="#;

    //     let (defs, filter) = parse(code);

    //     assert!(defs.is_empty());
    //     assert_eq!(
    //         filter,
    //         Filter::AssignmentExpression(
    //             Box::new(Filter::ObjIndex("foo".to_string())),
    //             "=".to_string(),
    //             Box::new(Filter::Hole)
    //         )
    //     );
    // }

    #[test]
    fn test_incomplete_optional_operator() {
        let code = r#".foo?"#;

        let (defs, filter) = parse(code);
        // TODO check !
        assert!(defs.is_empty());
        assert_eq!(filter, Filter::ObjIndex("foo".to_string()),);
    }

    #[test]
    fn test_if_expression2() {
        let code = r#"
            if true then
                1
            elif 3>2 then
                2
            
            end
        "#;

        let (defs, filter) = parse(code);

        assert!(defs.is_empty());
    }

    #[test]
    fn test_if_expression3() {
        let code = r#"
            if true then
                1

            end
        "#;

        let (defs, filter) = parse(code);

        assert!(defs.is_empty());
    }

    #[test]
    fn test_if_expression4() {
        let code = r#"
            if true then
                1
        "#;

        let (defs, filter) = parse(code);

        assert!(defs.is_empty());
    }
}
