//! Size distributions of generated programs and inputs.
use tjq_exec::{Filter, Json};
use tjq_testing::filtergen::{gen_filter, to_jq_source};
use tjq_testing::inhabit::inhabit;
use tjq_testing::jsongen::gen_json;
use tjq_testing::rng::Rng;
use tjq_testing::shapegen::gen_shape;

fn ast_nodes(f: &Filter) -> usize {
    1 + match f {
        Filter::Pipe(a, b) | Filter::Comma(a, b) => ast_nodes(a) + ast_nodes(b),
        Filter::BinOp(a, _, b) => ast_nodes(a) + ast_nodes(b),
        Filter::UnOp(_, a) | Filter::ObjIndex(a) | Filter::ArrayIndex(a) => ast_nodes(a),
        Filter::IfThenElse(c, t, e) => ast_nodes(c) + ast_nodes(t) + ast_nodes(e),
        Filter::Array(items) => items.iter().map(ast_nodes).sum(),
        Filter::Object(items) => items.iter().map(|(_, v)| ast_nodes(v)).sum(),
        _ => 0,
    }
}

fn json_nodes(j: &Json) -> usize {
    1 + match j {
        Json::Array(a) => a.iter().map(json_nodes).sum(),
        Json::Object(o) => o.iter().map(|(_, v)| json_nodes(v)).sum(),
        _ => 0,
    }
}

fn pct(sorted: &[usize], p: f64) -> usize {
    sorted[((sorted.len() as f64 - 1.0) * p) as usize]
}

fn report(label: &str, mut xs: Vec<usize>) {
    xs.sort();
    println!(
        "{label:28} p50={:>4}  p90={:>4}  p99={:>5}  max={:>6}",
        pct(&xs, 0.50),
        pct(&xs, 0.90),
        pct(&xs, 0.99),
        xs.last().unwrap()
    );
}

fn main() {
    const N: usize = 20_000;
    for depth in [3, 4, 5] {
        let mut rng = Rng::new(42);
        let (mut nodes, mut chars) = (vec![], vec![]);
        for _ in 0..N {
            let f = gen_filter(&mut rng, depth);
            nodes.push(ast_nodes(&f));
            chars.push(to_jq_source(&f).len());
        }
        report(&format!("program depth={depth} AST nodes"), nodes);
        report(&format!("program depth={depth} src chars"), chars);
    }

    // Inputs as the rig generates them: inhabitants of random shapes
    let mut rng = Rng::new(42);
    let (mut nodes, mut chars) = (vec![], vec![]);
    for _ in 0..N {
        let s = gen_shape(&mut rng, 3);
        if let Some(j) = inhabit(&s, &mut rng) {
            nodes.push(json_nodes(&j));
            chars.push(tjq_testing::to_json_string(&j).len());
        }
    }
    report("input (inhabited) JSON nodes", nodes);
    report("input (inhabited) chars", chars);

    // Raw random JSON (used for Blob/TVar-typed inputs)
    let mut rng = Rng::new(43);
    let (mut nodes, mut chars) = (vec![], vec![]);
    for _ in 0..N {
        let j = gen_json(&mut rng, 3);
        nodes.push(json_nodes(&j));
        chars.push(tjq_testing::to_json_string(&j).len());
    }
    report("input (random d=3) JSON nodes", nodes);
    report("input (random d=3) chars", chars);
}
