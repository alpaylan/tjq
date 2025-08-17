use std::fmt::{Display, Formatter};
use std::{array, default};
use std::{collections::HashMap, vec};

use serde_json::value;
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
    Empty,
    Error,
    BindingExpression,
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
            
           FilterKind::IfThenElse => {
                let n = cst.children.len();
                assert!(n >= 2, "IfThenElse must have at least cond and then");
                
                let cond: Filter = (&cst.children[0]).into();
                let then: Filter = (&cst.children[1]).into();
                
                let else_branch: Filter = if n > 2 {
                    (&cst.children[n - 1]).into()
                } else {
                    Filter::Dot
                };
                
                Filter::IfThenElse(
                    Box::new(cond),
                    Box::new(then),
                    Box::new(else_branch)
                )
            }
            FilterKind::Empty => {
                assert!(cst.children.len() == 0);
                Filter::Empty
            }
            FilterKind::Error => {
                assert!(cst.children.len() == 0);
                Filter::Error
            }
            FilterKind::BindingExpression => {
                assert!(cst.children.len() == 2);
                Filter::BindingExpression(
                    Box::new((&cst.children[0]).into()),
                    Box::new((&cst.children[1]).into()),
                )
            }
            FilterKind::Call => {
                //todo : validate 
                assert!(cst.children.len() >= 1);
                let args = if cst.children.len() > 1 {
                    Some(
                        cst.children[1..]
                            .iter()
                            .map(|c| c.into())
                            .collect(),
                    )
                } else {
                    None
                };
                Filter::Call(cst.children[0].value.to_string(), args)
            }
            FilterKind::ReduceExpression => todo!(),
            FilterKind::FunctionExpression => {
                assert!(cst.children.len() >= 1);
                todo!()
            }
            FilterKind::Bound => todo!(),
            


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
            FilterKind::Pipe => write!(f, "|"),
            FilterKind::Comma => write!(f, ","),
            FilterKind::ObjIndex => write!(f, ".[]"), //todo check this one
            FilterKind::ArrayIterator => todo!(),
            FilterKind::Null => write!(f, "null"),
            FilterKind::Boolean => todo!(),
            FilterKind::Number => todo!(),
            FilterKind::String => todo!(),
            FilterKind::Array => todo!(),
            FilterKind::BinOp(bin_op) => todo!(),
            FilterKind::UnOp(un_op) => todo!(),
            FilterKind::Variable => todo!(),
            FilterKind::Empty => write!(f, "empty"),
            FilterKind::Error => write!(f, "error"),
            FilterKind::BindingExpression => todo!(),
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
    pub(crate) fn string(range: Range, value: &'a str) -> Self {
        Self {
            kind: FilterKind::String,
            children: Vec::new(),
            range,
            value,
        }
    }
    pub(crate) fn object_index(range: Range, value: &'a str) -> Self {
        Self {
            kind: FilterKind::ObjIndex,
            children: Vec::new(),
            range,
            value,
        }
    }
    pub(crate) fn boolean(range: Range, value: &'a str) -> Self {
        Self {
            kind: FilterKind::Boolean,
            children: Vec::new(),
            range,
            value,
        }
    }
    pub(crate) fn null(range: Range) -> Self {
        Self {
            kind: FilterKind::Null,
            children: Vec::new(),
            range,
            value: "null",
        }
    }
    pub(crate) fn empty(range: Range) -> Self {
        Self {
            kind: FilterKind::Empty,
            children: Vec::new(),
            range,
            value: "empty",
        }
    }
    pub(crate) fn error(range: Range) -> Self {
        Self {
            kind: FilterKind::Error,
            children: Vec::new(),
            range,
            value: "error",
        }
    }
    pub(crate) fn call(range: Range, value: &'a str, name: & 'a str, args: Option<Vec<Cst<'a>>>) -> Self {
        let name = Cst::string(range, name);
        let children = if let Some(args) = args {
            vec![name].into_iter().chain(args.into_iter()).collect()
        } else {
            vec![name]
        };
        Self {
            kind: FilterKind::Call,
            children,
            range,
            value,
        }
    }
    pub(crate) fn variable(range: Range, value: &'a str) -> Self {
        Self {
            kind: FilterKind::Variable,
            children: Vec::new(),
            range,
            value,
        }
    }
    pub(crate) fn array(range: Range, values: Vec<Cst<'a>>, value: &'a str) -> Self {
        Self {
            kind: FilterKind::Array,
            children: values,
            range,
            value,
        }
    }
    pub(crate) fn object(range: Range, pairs: Vec<(Cst<'a>, Cst<'a>)>, value: &'a str) -> Self {
        Self {
            kind: FilterKind::Object,
            children: pairs
                .into_iter()
                .flat_map(|(key, val)| [key, val])
                .collect(),
            range,
            value,
        }
    }

    pub fn number(range: Range, value: &'a str) -> Self {
        Self {
            kind: FilterKind::Number,
            children: Vec::new(),
            range,
            value,
        }
    }

    pub fn bin_op(range: Range, lhs: Cst<'a>, op: BinOp, rhs: Cst<'a>, value: &'a str) -> Self {
        Self {
            kind: FilterKind::BinOp(op),
            children: vec![lhs, rhs],
            range,
            value,
        }
    }

    pub fn un_op(range: Range, op: UnOp, rhs: Cst<'a>, value: &'a str) -> Self {
        Self {
            kind: FilterKind::UnOp(op),
            children: vec![rhs],
            range,
            value,
        }
    }
    pub fn if_then_else(
        range: Range,
        cond: Cst<'a>,
        then: Cst<'a>,
        elifs: Vec<Cst<'a>>,
        else_: Cst<'a>,
        value: &'a str,
    ) -> Self {
        Self {
            kind: FilterKind::IfThenElse,
            children: vec![cond, then]
                .into_iter()
                .chain(elifs.into_iter())
                .chain(std::iter::once(else_))
                .collect(),
            range,
            value,
        }
    }
    pub fn bound(range: Range, value: &'a str, args: Vec<Cst<'a>>, body: Cst<'a>) -> Self {
        Self {
            kind: FilterKind::Bound,
            children: args.into_iter().chain(std::iter::once(body)).collect(),
            range,
            value,
        }
    }
    pub fn binding_expression(range: Range, value: &'a str, lhs: Cst<'a>, pat: Cst<'a>) -> Self {
        Self {
            kind: FilterKind::BindingExpression,
            children: vec![lhs, pat],
            range,
            value,
        }
    }
    pub fn function_expression(
        range: Range,
        value: &'a str,
        defs: HashMap<String, Cst<'a>>,
        final_expr: Cst<'a>,
    ) -> Self {
        Self {
            kind: FilterKind::FunctionExpression,
            children: defs
                .into_values()
                .chain(std::iter::once(final_expr))
                .collect(),
            range,
            value,
        }
    }
    pub fn array_iterator(range:Range, value : & 'a str , lhs: Cst<'a>) -> Self {
        Self {
            kind: FilterKind::ArrayIterator,
            children: vec![lhs],
            range,
            value,
        }
    }

    pub fn array_index(range: Range, value: &'a str, lhs: Cst<'a>, rhs: Cst<'a>) -> Self {
        Self {
            kind: FilterKind::ArrayIndex,
            children: vec![lhs, rhs],
            range,
            value,
        }
    }
    pub fn reduce_expression(range: Range, value: &'a str, var_cst: Cst<'a>, generator: Cst<'a>, init: Cst<'a>, update: Cst<'a>) -> Self {
        Self {
            kind: FilterKind::ReduceExpression,
            children: vec![var_cst, generator, init, update],
            range,
            value,
        }
    }
}

pub fn parse<'a>(code: &'a str) -> (HashMap<String, Cst<'a>>, Cst<'a>) {
    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(&tree_sitter_tjq::LANGUAGE.into())
        .expect("Error loading jq grammar");
    let tree = parser.parse(code, None).unwrap();

    // assert_eq!(tree.root_node().kind(), "program");

    print_ast(tree.root_node(), code, 0); // print AST
                                          //print_node_details(tree.root_node(), code, 0);

    let mut defs: HashMap<String, Cst<'a>> = HashMap::new();

    for i in 0..tree.root_node().child_count() - 1 {
        let child = tree.root_node().child(i).unwrap();
        match child.kind() {
            "function_definition" => {
                let (f, defs_) = parse_filter(code, child);
                defs_.into_iter().for_each(|(name, def)| {
                    defs.insert(name, def);
                });
                tracing::trace!("Parsed function definition: {}", f);
            }
            "comment" => {}
            _ => {
                println!("unexpected node in program: {}", child.kind());
            }
        }
    }

    let (f, _) = parse_filter(
        code,
        tree.root_node()
            .child(tree.root_node().child_count() - 1)
            .expect("root should have at one children"),
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
                let _ = parse_filter(code, child);
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

pub(crate) fn parse_filter<'a>(code: &'a str, root: Node<'_>) -> (Cst<'a>, Vec<(String, Cst<'a>)>) {
    tracing::trace!(
        "{}: {}",
        root.kind(),
        &code[root.range().start_byte..root.range().end_byte]
    );

    match root.kind() {
        "dot" => (Cst::dot(root.range()), vec![]),
        "sequence_expression" => {
            let (lhs, vl) = parse_filter(code, root.child(0).expect("sequence should have a lhs"));
            let (rhs, vr) = parse_filter(code, root.child(2).expect("sequence should have a rhs"));
            let v = vl.into_iter().chain(vr).collect();
            (
                Cst::pipe(
                    lhs,
                    rhs,
                    &code[root.range().start_byte..root.range().end_byte],
                    root.range(),
                ),
                v,
            )
        }
        "subscript_expression" => {
                let (lhs, vl) = parse_filter(code, root.child(0).expect("subscript should have a lhs"));
                let value = &code[root.range().start_byte..root.range().end_byte];
                if root.child_count() == 3 {
                    // Array iterator 
                    (Cst::array_iterator(root.range(), value, lhs), vl)
                } else {
                    //array index -> parse the rhs
                    let (rhs, vr) = parse_filter(
                        code,
                        root.child(2).expect("subscript should have a rhs"),
                    );
                    
                    let v = vl.into_iter().chain(vr).collect();
                    
                    (Cst::array_index(root.range(), value, lhs, rhs), v)
                }
            }
        "field" => {

                let key_ts = root
                    .child(1)
                    .expect("field access should have the second child as its field");

                let key_range = key_ts.range();
                let raw = &code[key_range.start_byte..key_range.end_byte];

                let key = if (raw.starts_with('"') && raw.ends_with('"'))
                    || (raw.starts_with('\'') && raw.ends_with('\''))
                {
                    &raw[1..raw.len() - 1]
                } else {
                    raw
                };

                // let key_node = Cst::string(key_range, key);
                //todo : determine whether we need a key node or not 
                (Cst::object_index(root.range(),key), vec![])

            }

        "field_id" => {
            let value = &code[root.range().start_byte..root.range().end_byte];
            (Cst::string(root.range(), value), vec![])
        }
        "identifier" => {
            let cst = match &code[root.range().start_byte..root.range().end_byte] {
                "true" => Cst::boolean(root.range(), "true"),
                "false" => Cst::boolean(root.range(), "false"),
                "null" => Cst::null(root.range()),
                "empty" => Cst::empty(root.range()),
                "error" => Cst::error(root.range()),
                "" => Cst::hole(root.range()),
                s => Cst::call(root.range(), s, s,None), 
            };
            (cst, vec![])
        }
        "variable" => {
            let name = &code[root.range().start_byte + 1..root.range().end_byte];
            (Cst::variable(root.range(), name), vec![])
        }
        "array" => {
            let val = &code[root.range().start_byte..root.range().end_byte];

            if root.child_count() == 2 {
                return (Cst::array(root.range(), vec![], val), vec![]);
            }
            
            let mut all_defs = vec![];
            let children: Vec<Cst> = (1..root.child_count())
                .step_by(2)
                .map(|i| {
                    let (child, child_defs) = parse_filter(
                        code,
                        root.child(i).expect("array should have a value"),
                    );
                    all_defs.extend(child_defs);
                    child
                })
                .collect();
            
            (Cst::array(root.range(), children, val), all_defs)
        }
        "object" => {
                let mut pairs: Vec<(Cst<'a>, Cst<'a>)> = vec![];
                let mut all_defs = vec![];
                
                for i in (1..root.child_count() - 1).step_by(2) {
                    let pair = root.child(i).unwrap();
                    let (key, key_defs) = parse_filter(code, pair.child(0).expect("pairs should have a key"));
                    let (value, value_defs) = parse_filter(
                        code,
                        pair.child(2).expect("pairs should have a value"),
                    );
                    
                    all_defs.extend(key_defs);
                    all_defs.extend(value_defs);
                    pairs.push((key, value));
                }

                let value = &code[root.range().start_byte..root.range().end_byte];

                (Cst::object(root.range(), pairs, value), all_defs)
        }
        "number" => {
            let value = &code[root.range().start_byte..root.range().end_byte];
            (Cst::number(root.range(), value), vec![])
        }
        "string" => {
            let s = &code[root.range().start_byte..root.range().end_byte];
            (Cst::string(root.range(), s), vec![])
        }
        "pipeline" => {
            let value = &code[root.range().start_byte..root.range().end_byte];
            let (lhs,vl) = parse_filter(code, root.child(0).expect("pipe should have a lhs"));
            let (rhs,vr) = parse_filter(code, root.child(2).expect("pipe should have a rhs"));
            let v = vl.into_iter().chain(vr).collect();
            (Cst::pipe(lhs, rhs, value, root.range()), v)
        }
        "binary_expression" => {
            let value = &code[root.range().start_byte..root.range().end_byte];

            let (lhs,vl) = parse_filter(
                code,
                root.child(0).expect("binary expression should have a lhs"),
                
            );
            let (rhs,vr) = parse_filter(
                code,
                root.child(2).expect("binary expression should have a rhs")
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
            let v = vl.into_iter().chain(vr).collect();
            (Cst::bin_op(root.range(), lhs, op, rhs, value), v)
        }
        "unary_expression" => {
            let value = &code[root.range().start_byte..root.range().end_byte];
            let (rhs,v) = parse_filter(
                code,
                root.child(1).expect("unary expression should have a rhs"),
            );
            let op = match &code
                [root.child(0).unwrap().range().start_byte..root.child(0).unwrap().range().end_byte]
            {
                "-" => UnOp::Neg,
                _ => panic!("unknown unary operator"),
            };

            (Cst::un_op(root.range(), op, rhs, value),v)
        }
        "true" => (Cst::boolean(root.range(), "true"),vec![]),
        "false" => (Cst::boolean(root.range(), "false"),vec![]),
        "null" => (Cst::null(root.range()), vec![]),
        "parenthesized_expression" => parse_filter(
            code,
            root.child(1)
                .expect("paranthesized expression should have a value"),
        ),
        "if_expression" => {
                let value = &code[root.range().start_byte..root.range().end_byte];
                
                let (cond, vc) = parse_filter(
                    code,
                    root.child(1).expect("if expression should have a condition"),
                );
                
                let (then, vt) = parse_filter(
                    code,
                    root.child(3).expect("if expression should have a then"),
                );
                
                let mut velif = vec![];
            let elifs: Vec<Cst<'a>> = (4..root.child_count() - 2)
                .map(|i| {
                    let elif = root.child(i).expect("if expression should have an elif");
                    let (elif_cond, vec_elif_cond) = parse_filter(
                        code,
                        elif.child(1).expect("elif expression should have a condition"),
                    );
                    let (elif_then, vec_elif_then) = parse_filter(
                        code,
                        elif.child(3).expect("elif expression should have a then"),
                    );
                    
                    velif.extend(vec_elif_cond);
                    velif.extend(vec_elif_then);
                    
                    Cst::if_then_else(
                        elif.range(), 
                        elif_cond, 
                        elif_then, 
                        vec![], 
                        Cst::dot(elif.range()),
                        value
                    )
                })
                .collect();

                let tail = root
                    .child(root.child_count() - 2)
                    .expect("if expression should end with 'end' or 'else <expr> end'");
                let tail_str = &code[tail.range().start_byte..tail.range().end_byte];

                let (else_, ve) = if tail_str == "end" {
                    (Cst::dot(tail.range()), vec![])
                } else {
                    // println!("{}", tail_str);
                    parse_filter(code, tail)
                };
                //todo fix this up
                let v = vc.into_iter()
                    .chain(vt)
                    .chain(velif)
                    .chain(ve)
                    .collect();

                (Cst::if_then_else(root.range(), cond, then, elifs, else_, value), v)
            }
        "else_expression" => parse_filter(
            code,
            root.child(1).expect("else expression should have a value"),
        ),

        "call_expression" => {
            let value = &code[root.range().start_byte..root.range().end_byte];
            let name = &code[root.child(0).unwrap().range().start_byte
                ..root.child(0).unwrap().range().end_byte];
            let args = root.child(1);
            if let Some(args) = args {
                let mut vargs = vec![];
                let parsed_args: Vec<Cst> = (1..args.child_count())
                    .step_by(2)
                    .map(|i| {
                        let (arg, varg) = parse_filter(
                            code,
                            args.child(i).expect("call expression should have arguments"),
                        );
                        vargs.extend(varg);
                        arg
                    })
                    .collect();
                
                (Cst::call(root.range(), value, name,Some(parsed_args)), vargs)
            } else {
                (Cst::call(root.range(), value, name,None), vec![])
            }
        },
        "function_definition" => {
            let value = &code[root.range().start_byte..root.range().end_byte];
            let name = code[root.child(1).unwrap().range().start_byte
                ..root.child(1).unwrap().range().end_byte]
                .to_string();
            
            let args = root
                .child(2)
                .expect("function definition should have arguments");
            
            let mut vargs = vec![];
            let parsed_args: Vec<Cst> = (1..args.child_count())
                .step_by(2)
                .map(|i| {
                    let (arg, varg) = parse_filter(
                        code,
                        args.child(i).expect("function definition should have an argument"),
                    );
                    vargs.extend(varg);
                    arg
                })
                .collect();
            
            let (body, vbody) = parse_filter(
                code,
                root.child(root.child_count() - 2)
                    .expect("function definition should have a body"),
            );
            
            let function_cst = Cst::bound(root.range(), value, parsed_args, body);
            
            let mut v = vargs;
            v.extend(vbody);
            v.push((name, function_cst));
            
            (Cst::dot(root.range()), v)
        },

        "function_expression" => {
            let mut final_expr = Cst::dot(root.range());
            let mut inner_defs = HashMap::new();
            let mut all_defs = vec![];

            for i in 0..root.child_count() {
                let child = root.child(i).unwrap();

                match child.kind() {
                    "function_definition" => {
                        let (_, vdef) = parse_filter(code, child);
                        for (name, def) in vdef {
                            inner_defs.insert(name, def);
                        }
                    }
                    _ => {
                        let (expr, vexpr) = parse_filter(code, child);
                        final_expr = expr;
                        all_defs.extend(vexpr);
                    }
                }
            }

            (
                Cst::function_expression(
                    root.range(),
                    &code[root.range().start_byte..root.range().end_byte],
                    inner_defs,
                    final_expr,
                ),
                all_defs,
            )
        }
        "binding_expression" => {
            let (lhs,vl) = parse_filter(code, root.child(0).unwrap());
            let (pat,vpat) = parse_filter(code, root.child(2).unwrap());
            
            let v = vl.into_iter().chain(vpat).collect();
            
            (
                Cst::binding_expression(
                    root.range(),
                    &code[root.range().start_byte..root.range().end_byte],
                    lhs,
                    pat,
                ),
                v,
            )
        }
        "optional_expression" => {
            let field = root
                .child(0)
                .expect("optional expression should have the first child as its field");

            let (identifier, vfield) = parse_filter(
                code,
                field
                    .child(1)
                    .expect("field access should have the second child as its field"),
            );
            
            (Cst::object_index(root.range(), &code[root.range().start_byte..root.range().end_byte]), vfield)
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

            let (generator, vgen) = parse_filter(code, source_node);

            let vtxt = &code[var_node.range().start_byte..var_node.range().end_byte];
            let var_name_str = vtxt
                .strip_prefix('$')
                .unwrap_or_else(|| panic!("reduce: expected variable"));
            
            let var_cst = Cst::variable(var_node.range(), var_name_str);

            let init_node = root
                .child(3)
                .expect("reduce: missing initializer expression");
            let upd_node = root.child(5).expect("reduce: missing update expression");

            let (init, vinit) = parse_filter(code, init_node);
            let (update, vupdate) = parse_filter(code, upd_node);

            let v = vgen
                .into_iter()
                .chain(vinit)
                .chain(vupdate)
                .collect::<Vec<_>>();

            (
                Cst::reduce_expression(
                    root.range(),
                    &code[root.range().start_byte..root.range().end_byte],
                    var_cst,
                    generator,
                    init,
                    update,
                ),
                v,
            )
        }
        "hole" => (Cst::hole(root.range()), vec![]),
        "assignment_expression" => todo!(),
        "foreach_expression" => todo!(),
        "field_expression" => {
                // PExp '.' [<str>|<field>]
                todo!()

        }
        "slice_expression" => todo!(),
        _ => {
            tracing::warn!(
                "unknown filter {} {}",
                root.kind(),
                code[root.range().start_byte..root.range().end_byte].to_string()
            );
            panic!(
                "unknown filter {} {}",
                root.kind(),
                code[root.range().start_byte..root.range().end_byte].to_string()
            );
        
        }
    }
}

#[cfg(test)]
mod tests {

    use std::process::CommandEnvs;

    use tracing_subscriber::EnvFilter;

    use super::*;

}

//     #[test]
//     fn test_parse() {
//         let code = r#"
//             def add(a; b): a + b;
//             add(1; 2)
//         "#;

//         let (defs, filter) = parse(code);

//         assert_eq!(
//             defs.get("add").unwrap().clone(),
//             Filter::Bound(
//                 vec!["a".to_string(), "b".to_string()],
//                 Box::new(Filter::BinOp(
//                     Box::new(Filter::Call("a".to_string(), None)),
//                     BinOp::Add,
//                     Box::new(Filter::Call("b".to_string(), None))
//                 ))
//             )
//         );

//         assert_eq!(
//             filter,
//             Filter::Call(
//                 "add".to_string(),
//                 Some(vec![Filter::Number(1.0), Filter::Number(2.0)])
//             )
//         );
//     }

//     #[test]
//     fn test_parse_filter() {
//         let code = r#"
//             1 + 2
//         "#;

//         let (defs, filter) = parse(code);

//         assert_eq!(defs, HashMap::new());

//         assert_eq!(
//             filter,
//             Filter::BinOp(
//                 Box::new(Filter::Number(1.0)),
//                 BinOp::Add,
//                 Box::new(Filter::Number(2.0))
//             )
//         );
//     }

//     #[test]
//     fn test_parse_function_definition() {
//         let code = r#"
//             (def f:  .[] ;  .) | f
//         "#;
//         let (defs, filter) = parse(code);
//         assert!(defs.is_empty());
//         assert_eq!(
//             filter,
//             Filter::Pipe(
//                 Box::new(Filter::FunctionExpression(
//                     HashMap::from([(
//                         "f".to_string(),
//                         Filter::Bound(vec![], Box::new(Filter::ArrayIterator))
//                     )]),
//                     Box::new(Filter::Dot)
//                 )),
//                 Box::new(Filter::Call("f".to_string(), None))
//             )
//         );
//     }

//     #[test]
//     fn test_parse_variable() {
//         let code = r#"
//             def main:
//                 def add(a; b): a + b;
//                 def sub(a; b): a - b;
//                 def mul(a; b): a * b;
//                 add(1; 2);
//             sub(3; 1)
//         "#;
//         let (defs, filter) = parse(code);
//         assert_eq!(
//             defs.get("main").unwrap().clone(),
//             Filter::Bound(
//                 vec![],
//                 Box::new(Filter::FunctionExpression(
//                     HashMap::from([
//                         (
//                             "add".to_string(),
//                             Filter::Bound(
//                                 vec!["a".to_string(), "b".to_string()],
//                                 Box::new(Filter::BinOp(
//                                     Box::new(Filter::Call("a".to_string(), None)),
//                                     BinOp::Add,
//                                     Box::new(Filter::Call("b".to_string(), None))
//                                 ))
//                             )
//                         ),
//                         (
//                             "sub".to_string(),
//                             Filter::Bound(
//                                 vec!["a".to_string(), "b".to_string()],
//                                 Box::new(Filter::BinOp(
//                                     Box::new(Filter::Call("a".to_string(), None)),
//                                     BinOp::Sub,
//                                     Box::new(Filter::Call("b".to_string(), None))
//                                 ))
//                             )
//                         ),
//                         (
//                             "mul".to_string(),
//                             Filter::Bound(
//                                 vec!["a".to_string(), "b".to_string()],
//                                 Box::new(Filter::BinOp(
//                                     Box::new(Filter::Call("a".to_string(), None)),
//                                     BinOp::Mul,
//                                     Box::new(Filter::Call("b".to_string(), None))
//                                 ))
//                             )
//                         ),
//                     ]),
//                     Box::new(Filter::Call(
//                         "add".to_string(),
//                         Some(vec![Filter::Number(1.0), Filter::Number(2.0)])
//                     ))
//                 ))
//             )
//         );
//         assert_eq!(
//             filter,
//             Filter::Call(
//                 "sub".to_string(),
//                 Some(vec![Filter::Number(3.0), Filter::Number(1.0)])
//             )
//         );
//     }
//     #[test]
//     fn test_if_expression() {
//         let code = r#"
//             if true then
//                 1
//             else
//                 2
//                 end
//         "#;

//         let (defs, filter) = parse(code);

//         assert!(defs.is_empty());
//         assert_eq!(
//             filter,
//             Filter::IfThenElse(
//                 Box::new(Filter::Boolean(true)),
//                 Box::new(Filter::Number(1.0)),
//                 Box::new(Filter::Number(2.0))
//             )
//         );
//     }

//     #[test]
//     fn test_binding_expression() {
//         let code = r#"
//            . as $item 
//         "#;
//         let (defs, filter) = parse(code);

//         assert_eq!(
//             filter,
//             Filter::BindingExpression(
//                 Box::new(Filter::Dot),
//                 Box::new(Filter::Variable("item".to_string()))
//             )
//         );
//     }

//     #[test]
//     fn test_binding_expression2() {
//         let code = r#"
//            1 as $item  | $item
//         "#;
//         let (defs, filter) = parse(code);

//         assert_eq!(
//             filter,
//             Filter::Pipe(
//                 (Box::new(Filter::BindingExpression(
//                     Box::new(Filter::Number(1.0)),
//                     Box::new(Filter::Variable("item".to_string()))
//                 ))),
//                 Box::new(Filter::Variable("item".to_string()),)
//             )
//         );
//     }

//     #[test]
//     fn test_binding_expression3() {
//         let code = r#"
//            [1,2,3] as $item  | $item
//         "#;
//         let (defs, filter) = parse(code);

//         assert_eq!(
//             filter,
//             Filter::Pipe(
//                 (Box::new(Filter::BindingExpression(
//                     Box::new(Filter::Array(vec![
//                         Filter::Number(1.0),
//                         Filter::Number(2.0),
//                         Filter::Number(3.0)
//                     ])),
//                     Box::new(Filter::Variable("item".to_string()))
//                 ))),
//                 Box::new(Filter::Variable("item".to_string()),)
//             )
//         );
//     }

//     #[test]
//     fn test_incomplete_binop() {
//         let code = r#"1 + "#;

//         let (defs, filter) = parse(code);

//         assert!(defs.is_empty());
//         assert_eq!(
//             filter,
//             Filter::BinOp(
//                 Box::new(Filter::Number(1.0)),
//                 BinOp::Add,
//                 Box::new(Filter::Hole)
//             )
//         );
//     }

//     #[test]
//     fn test_incomplete_binop2() {
//         let code = r#"1 == "#;

//         let (defs, filter) = parse(code);

//         assert!(defs.is_empty());
//         assert_eq!(
//             filter,
//             Filter::BinOp(
//                 Box::new(Filter::Number(1.0)),
//                 BinOp::Eq,
//                 Box::new(Filter::Hole)
//             )
//         );
//     }

//     #[test]
//     fn test_incomplete_pipe() {
//         let _ = tracing_subscriber::fmt()
//             .with_target(false)
//             .with_thread_ids(false)
//             .with_thread_names(false)
//             .with_file(true)
//             .with_line_number(true)
//             .with_level(true)
//             .without_time()
//             .with_env_filter(EnvFilter::from_default_env())
//             .try_init();
//         let code = r#"1 | "#;
//         let inferred = r#"1 | ??"#;

//         let (_, filter) = parse(code);
//         let (_, expected) = parse(inferred);
//         assert_eq!(filter, expected);
//     }

//     #[test]
//     fn test_incomplete_pipe2() {
//         let code = r#" ?? | 1 "#;

//         let (defs, filter) = parse(code);

//         assert!(defs.is_empty());
//         assert_eq!(
//             filter,
//             Filter::Pipe(Box::new(Filter::Hole), Box::new(Filter::Number(1.0)))
//         );
//     }

//     #[test]
//     fn test_incomplete_comma() {
//         let code = r#"1, "#;

//         let (defs, filter) = parse(code);

//         assert!(defs.is_empty());
//         assert_eq!(
//             filter,
//             Filter::Comma(Box::new(Filter::Number(1.0)), Box::new(Filter::Hole))
//         );
//     }

//     #[test]
//     fn test_abs() {
//         let (_, filter) = parse("if . < 0 then - . end");
//         assert_eq!(
//             filter,
//             Filter::IfThenElse(
//                 Box::new(Filter::BinOp(
//                     Box::new(Filter::Dot),
//                     BinOp::Lt,
//                     Box::new(Filter::Number(0.0))
//                 )),
//                 Box::new(Filter::UnOp(UnOp::Neg, Box::new(Filter::Dot))),
//                 Box::new(Filter::Dot)
//             )
//         );
//     }

//     #[test]
//     fn test_incomplete_function_definition() {
//         let code = r#"
//             def f(a; b):
//         "#;

//         let (defs, filter) = parse(code);

//         assert_eq!(
//             defs.get("f"),
//             Some(&Filter::Bound(
//                 vec!["a".to_string(), "b".to_string()],
//                 Box::new(Filter::Hole)
//             ))
//         );
//         assert_eq!(filter, Filter::Hole);
//     }

//     #[test]
//     fn test_incomplete_function_definition2() {
//         let code = r#"
//             def f(a; b):
//                 a*b
//         "#;

//         let (defs, filter) = parse(code);

//         assert_eq!(
//             defs.get("f"),
//             Some(&Filter::Bound(
//                 vec!["a".to_string(), "b".to_string()],
//                 Box::new(Filter::BinOp(
//                     Box::new(Filter::Call("a".to_string(), None)),
//                     BinOp::Mul,
//                     Box::new(Filter::Call("b".to_string(), None))
//                 ))
//             ))
//         );
//         // assert_eq!(filter, Filter::Hole);
//         //check this!
//     }

//     #[test]
//     fn test_incomplete_function_expression() {
//         let code = r#"
//             (def f(a; b): a + b; )
//         "#;

//         let (defs, filter) = parse(code);

//         assert!(defs.is_empty());
//         assert_eq!(
//             filter,
//             Filter::FunctionExpression(
//                 HashMap::from([(
//                     "f".to_string(),
//                     Filter::Bound(
//                         vec!["a".to_string(), "b".to_string()],
//                         Box::new(Filter::BinOp(
//                             Box::new(Filter::Call("a".to_string(), None)),
//                             BinOp::Add,
//                             Box::new(Filter::Call("b".to_string(), None))
//                         ))
//                     )
//                 )]),
//                 Box::new(Filter::Hole)
//             )
//         );
//     }

//     #[test]
//     fn test_incomplete_nested_function_expression() {
//         let code = r#"
//             (def f(a; b): 
//                 def g(c; d):
//             )

//         "#;

//         let (defs, filter) = parse(code);

//         assert!(defs.is_empty());
//         assert_eq!(
//             filter,
//             Filter::FunctionExpression(
//                 HashMap::from([(
//                     "f".to_string(),
//                     Filter::Bound(
//                         vec!["a".to_string(), "b".to_string()],
//                         Box::new(Filter::FunctionExpression(
//                             HashMap::from([(
//                                 "g".to_string(),
//                                 Filter::Bound(
//                                     vec!["c".to_string(), "d".to_string()],
//                                     Box::new(Filter::Hole)
//                                 )
//                             )]),
//                             Box::new(Filter::Hole)
//                         ))
//                     )
//                 )]),
//                 Box::new(Filter::Hole)
//             )
//         );
//     }

//     #[test]
//     fn test_incomplete_nested_function_expression2() {
//         let code = r#"
//             (def f(a; b): 
//                 def g(c; d):
//                     c + d;
//             )

//         "#;

//         let (defs, filter) = parse(code);

//         assert!(defs.is_empty());
//         assert_eq!(
//             filter,
//             Filter::FunctionExpression(
//                 HashMap::from([(
//                     "f".to_string(),
//                     Filter::Bound(
//                         vec!["a".to_string(), "b".to_string()],
//                         Box::new(Filter::FunctionExpression(
//                             HashMap::from([(
//                                 "g".to_string(),
//                                 Filter::Bound(
//                                     vec!["c".to_string(), "d".to_string()],
//                                     Box::new(Filter::BinOp(
//                                         Box::new(Filter::Call("c".to_string(), None)),
//                                         BinOp::Add,
//                                         Box::new(Filter::Call("d".to_string(), None))
//                                     ))
//                                 )
//                             )]),
//                             Box::new(Filter::Hole)
//                         ))
//                     )
//                 )]),
//                 Box::new(Filter::Hole)
//             )
//         );
//     }

//     #[test]
//     fn test_incomplete_nested_function_expression3() {
//         let code = r#"
//             (def f(a; b): 
//                 def g(c; d):
//                     c + d;
//                 e + f;
//             )

//         "#;

//         let (defs, filter) = parse(code);

//         assert!(defs.is_empty());
//         assert_eq!(
//             filter,
//             Filter::FunctionExpression(
//                 HashMap::from([(
//                     "f".to_string(),
//                     Filter::Bound(
//                         vec!["a".to_string(), "b".to_string()],
//                         Box::new(Filter::FunctionExpression(
//                             HashMap::from([(
//                                 "g".to_string(),
//                                 Filter::Bound(
//                                     vec!["c".to_string(), "d".to_string()],
//                                     Box::new(Filter::BinOp(
//                                         Box::new(Filter::Call("c".to_string(), None)),
//                                         BinOp::Add,
//                                         Box::new(Filter::Call("d".to_string(), None))
//                                     ))
//                                 )
//                             )]),
//                             Box::new(Filter::BinOp(
//                                 Box::new(Filter::Call("e".to_string(), None)),
//                                 BinOp::Add,
//                                 Box::new(Filter::Call("f".to_string(), None))
//                             ))
//                         ))
//                     )
//                 )]),
//                 Box::new(Filter::Hole)
//             )
//         );
//     }

//     #[test]
//     fn test_incomplete_if_then_else() {
//         let code = r#"
//             if true then
//                 1
//             else  ?? end
            
//         "#;

//         let (defs, filter) = parse(code);

//         assert!(defs.is_empty());
//         assert_eq!(
//             filter,
//             Filter::IfThenElse(
//                 Box::new(Filter::Boolean(true)),
//                 Box::new(Filter::Number(1.0)),
//                 Box::new(Filter::Hole)
//             )
//         );
//     }

//     #[test]
//     fn test_incomplete_if_then_else2() {
//         let code = r#"
//             if true then
//                 1
//         "#;

//         let (defs, filter) = parse(code);

//         assert!(defs.is_empty());
//         assert_eq!(
//             filter,
//             Filter::IfThenElse(
//                 Box::new(Filter::Boolean(true)),
//                 Box::new(Filter::Number(1.0)),
//                 Box::new(Filter::Hole)
//             )
//         );
//     }

//     #[test]
//     fn test_incomplete_if_then_else3() {
//         let code = r#"
//             if true then end
//         "#;

//         let (defs, filter) = parse(code);

//         assert!(defs.is_empty());
//         assert_eq!(
//             filter,
//             Filter::IfThenElse(
//                 Box::new(Filter::Boolean(true)),
//                 Box::new(Filter::Hole),
//                 Box::new(Filter::Hole)
//             )
//         );
//     }

//     #[test]
//     fn test_incomplete_if_then_else4() {
//         let code = r#"
//             if true 
//         "#;

//         let (defs, filter) = parse(code);

//         assert!(defs.is_empty());
//         assert_eq!(
//             filter,
//             Filter::IfThenElse(
//                 Box::new(Filter::Boolean(true)),
//                 Box::new(Filter::Hole),
//                 Box::new(Filter::Hole)
//             )
//         );
//     }

//     #[test]
//     fn test_incomplete_if_then_else5() {
//         let code = r#"
//             if
//         "#;

//         let (defs, filter) = parse(code);

//         assert!(defs.is_empty());
//         assert_eq!(
//             filter,
//             Filter::IfThenElse(
//                 Box::new(Filter::Hole),
//                 Box::new(Filter::Hole),
//                 Box::new(Filter::Hole)
//             )
//         );
//     }

//     #[test]
//     fn test_hole_token() {
//         let code = r#" . | ?? "#;

//         let (defs, filter) = parse(code);
//         assert!(defs.is_empty());
//         assert_eq!(
//             filter,
//             Filter::Pipe(Box::new(Filter::Dot), Box::new(Filter::Hole))
//         );
//     }
//     #[test]
//     fn test_reduce_expression() {
//         let code = r#"
//             reduce .[] as $item ( 0; . + 1 )
//         "#;

//         let (defs, filter) = parse(code);

//         assert!(defs.is_empty());
//         assert_eq!(
//             filter,
//             Filter::ReduceExpression(
//                 "item".to_string(),
//                 Box::new(Filter::ArrayIterator),
//                 Box::new(Filter::Number(0.0)),
//                 Box::new(Filter::BinOp(
//                     Box::new(Filter::Dot),
//                     BinOp::Add,
//                     Box::new(Filter::Number(1.0))
//                 ))
//             )
//         );
//     }
//     #[test]

//     fn test_incomplete_unop() {}

//     #[test]
//     fn test_incomplete_reduce() {
//         let code = r#"
//             reduce .[] as $item 
//         "#;

//         let (defs, filter) = parse(code);

//         assert!(defs.is_empty());
//         assert_eq!(
//             filter,
//             Filter::ReduceExpression(
//                 "item".to_string(),
//                 Box::new(Filter::Number(0.0)),
//                 Box::new(Filter::Hole),
//                 Box::new(Filter::Hole)
//             )
//         );
//     }

//     #[test]
//     fn test_incomplete_reduce2() {
//         let code = r#"
//             reduce  (;  . + 1 )
//         "#;

//         let (defs, filter) = parse(code);

//         assert!(defs.is_empty());
//         assert_eq!(
//             filter,
//             Filter::ReduceExpression(
//                 "".to_string(),
//                 Box::new(Filter::Hole),
//                 Box::new(Filter::Hole),
//                 Box::new(Filter::BinOp(
//                     Box::new(Filter::Dot),
//                     BinOp::Add,
//                     Box::new(Filter::Number(1.0))
//                 )),
//             )
//         );
//     }

//     #[test]
//     fn test_incomplete_reduce3() {
//         let code = r#"reduce .[] as $item (  )"#;
//         let expected = r#"reduce .[] as $item (??; ??)"#;

//         let (_, filter) = parse(code);
//         let (_, expected) = parse(expected);
//         //TODO check this (incomplete?)
//         assert_eq!(filter, expected);
//     }

//     #[test]

//     fn test_incomplete_binding() {
//         let code = r#"
//             . as 
//         "#;

//         let (defs, filter) = parse(code);

//         assert!(defs.is_empty());
//         assert_eq!(
//             filter,
//             Filter::BindingExpression(Box::new(Filter::Dot), Box::new(Filter::Hole))
//         );
//     }

//     #[test]

//     fn test_incomplete_binding2() {
//         let code = r#"
//             as $item 
//         "#;

//         let (defs, filter) = parse(code);

//         assert!(defs.is_empty());
//         assert_eq!(
//             filter,
//             Filter::BindingExpression(Box::new(Filter::Hole), Box::new(Filter::Hole))
//         );
//     }

//     #[test]
//     fn test_incomplete_array() {
//         let code = r#"
//             [1,2,3,4
//         "#;

//         let (defs, filter) = parse(code);

//         assert!(defs.is_empty());
//         assert_eq!(
//             filter,
//             Filter::Array(vec![
//                 Filter::Number(1.0),
//                 Filter::Number(2.0),
//                 Filter::Number(3.0),
//                 Filter::Number(4.0),
//             ])
//         );
//     }

//     #[test]
//     fn test_call_expression() {
//         let code = r#"
//             add(1; 2)
//         "#;

//         let (defs, filter) = parse(code);

//         assert!(defs.is_empty());
//         assert_eq!(
//             filter,
//             Filter::Call(
//                 "add".to_string(),
//                 Some(vec![Filter::Number(1.0), Filter::Number(2.0)])
//             )
//         );
//     }

//     #[test]
//     fn test_incomplete_call_expression() {
//         let code = r#"
//             add(1; )
//         "#;

//         let (defs, filter) = parse(code);

//         assert!(defs.is_empty());
//         assert_eq!(
//             filter,
//             Filter::Call(
//                 "add".to_string(),
//                 Some(vec![Filter::Number(1.0), Filter::Hole])
//             )
//         );
//     }
//     #[test]
//     fn test_incomplete_call_expression2() {
//         let code = r#"
//             add(; 1)
//         "#;

//         let (defs, filter) = parse(code);

//         assert!(defs.is_empty());
//         assert_eq!(
//             filter,
//             Filter::Call(
//                 "add".to_string(),
//                 Some(vec![Filter::Hole, Filter::Number(1.0)])
//             )
//         );
//     }

//     #[test]
//     fn test_subscript_expression() {
//         let code = r#"
//            . | .[2]
//         "#;

//         let (defs, filter) = parse(code);
//         assert!(defs.is_empty());
//         assert_eq!(
//             filter,
//             Filter::Pipe(Box::new(Filter::Dot), Box::new(Filter::ArrayIndex(2)))
//         );
//     }
//     // #[test]
//     // fn test_array_slice() {
//     //     let code = r#"
//     //         .[1:3]
//     //     "#;

//     //     let (defs, filter) = parse(code);
//     //     assert!(defs.is_empty());
//     //     assert_eq!(
//     //         filter,
//     //         Filter::SliceExpression(1, 3)
//     //     );
//     // }
//     #[test]
//     fn test_incomplete_unary_expression() {
//         let code = r#"-"#;

//         let (defs, filter) = parse(code);

//         assert!(defs.is_empty());
//         assert_eq!(filter, Filter::UnOp(UnOp::Neg, Box::new(Filter::Hole)));
//     }

//     #[test]
//     fn test_incomplete_object() {
//         let code = r#"{"name": "john", "age":"#;

//         let (defs, filter) = parse(code);

//         assert!(defs.is_empty());
//         assert_eq!(
//             filter,
//             Filter::Object(vec![
//                 (
//                     Filter::String("name".to_string()),
//                     Filter::String("john".to_string())
//                 ),
//                 (Filter::String("age".to_string()), Filter::Hole)
//             ])
//         );
//     }

//     #[test]
//     fn test_incomplete_object_key() {
//         let code = r#"{: "value"}"#;

//         let (defs, filter) = parse(code);

//         assert!(defs.is_empty());
//         assert_eq!(
//             filter,
//             Filter::Object(vec![(Filter::Hole, Filter::String("value".to_string()))])
//         );
//     }

//     #[test]
//     fn test_incomplete_parenthesized() {
//         let code = r#"("#;

//         let (defs, filter) = parse(code);

//         assert!(defs.is_empty());
//         assert_eq!(filter, Filter::Hole);
//     }

//     #[test]
//     fn test_incomplete_parenthesized_with_content() {
//         let code = r#"(1 + "#;

//         let (defs, filter) = parse(code);

//         assert!(defs.is_empty());
//         assert_eq!(
//             filter,
//             Filter::BinOp(
//                 Box::new(Filter::Number(1.0)),
//                 BinOp::Add,
//                 Box::new(Filter::Hole)
//             )
//         );
//     }
//     #[test]
//     fn test_incomplete_subscript() {
//         let code = r#".[1"#;

//         let (defs, filter) = parse(code);
//         //TODO : determine the output
//         assert!(defs.is_empty());
//         // This should handle incomplete array access
//         assert_eq!(filter, Filter::ArrayIndex(1));
//     }

//     // #[test]
//     // fn test_incomplete_try_catch() {
//     //     let code = r#"try"#;

//     //     let (defs, filter) = parse(code);

//     //     assert!(defs.is_empty());
//     //     // This should parse as a try expression with missing body
//     //     assert_eq!(
//     //         filter,
//     //         Filter::TryExpression(Box::new(Filter::Hole), None)
//     //     );
//     // }

//     // #[test]
//     // fn test_incomplete_try2() {
//     //     let code = r#"try .foo catch"#;

//     //     let (defs, filter) = parse(code);

//     //     assert!(defs.is_empty());
//     //     assert_eq!(
//     //         filter,
//     //         Filter::TryExpression(
//     //             Box::new(Filter::ObjIndex("foo".to_string())),
//     //             Some(Box::new(Filter::Hole))
//     //         )
//     //     );
//     // }

//     #[test]
//     fn test_multiple_incomplete_pipes() {
//         let code = r#". | | ."#;

//         let (defs, filter) = parse(code);

//         assert!(defs.is_empty());
//         assert_eq!(
//             filter,
//             Filter::Pipe(
//                 Box::new(Filter::Dot),
//                 Box::new(Filter::Pipe(Box::new(Filter::Hole), Box::new(Filter::Dot)))
//             )
//         );
//     }

//     #[test]
//     fn test_incomplete_sequence() {
//         let code = r#"1, 2,"#;
//         let expected = r#"1, 2, ??"#;
//         let (_, filter) = parse(code);
//         let (_, expected) = parse(expected);
//         //TODO check this (incomplete?)
//         assert_eq!(filter, expected);
//     }

//     #[test]
//     fn test_incomplete_nested_arrays() {
//         let code = r#"[[1, 2], [3,"#;

//         let (defs, filter) = parse(code);

//         assert!(defs.is_empty());
//         assert_eq!(
//             filter,
//             Filter::Array(vec![
//                 Filter::Array(vec![Filter::Number(1.0), Filter::Number(2.0)]),
//                 Filter::Array(vec![Filter::Number(3.0), Filter::Hole])
//             ])
//         );
//     }

//     #[test]
//     fn test_incomplete_variable_reference() {
//         let code = r#"$"#;

//         let (defs, filter) = parse(code);

//         assert!(defs.is_empty());
//         assert_eq!(filter, Filter::Variable("".to_string()));
//     }

//     // #[test]
//     // fn test_incomplete_assignment() {
//     //     let code = r#".foo ="#;

//     //     let (defs, filter) = parse(code);

//     //     assert!(defs.is_empty());
//     //     assert_eq!(
//     //         filter,
//     //         Filter::AssignmentExpression(
//     //             Box::new(Filter::ObjIndex("foo".to_string())),
//     //             "=".to_string(),
//     //             Box::new(Filter::Hole)
//     //         )
//     //     );
//     // }

//     #[test]
//     fn test_incomplete_optional_operator() {
//         let code = r#".foo?"#;

//         let (defs, filter) = parse(code);
//         // TODO check !
//         assert!(defs.is_empty());
//         assert_eq!(filter, Filter::ObjIndex("foo".to_string()),);
//     }

//     #[test]
//     fn test_if_expression2() {
//         let code = r#"
//             if true then
//                 1
//             elif 3>2 then
//                 2
            
//             end
//         "#;

//         let (defs, filter) = parse(code);

//         assert!(defs.is_empty());
//     }

//     #[test]
//     fn test_if_expression3() {
//         let code = r#"
//             if true then
//                 1

//             end
//         "#;

//         let (defs, filter) = parse(code);

//         assert!(defs.is_empty());
//     }

//     #[test]
//     fn test_if_expression4() {
//         let code = r#"
//             if true then
//                 1
//         "#;

//         let (defs, filter) = parse(code);

//         assert!(defs.is_empty());
//     }
// }
