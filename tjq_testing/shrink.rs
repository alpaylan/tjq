//! Shrinking for failing cases: given a (program, input) pair and a
//! predicate "still fails", greedily minimize both until no candidate
//! reduction preserves the failure.
//!
//! Candidates are ordered aggressive-first (delta-debugging style: whole
//! replacements, then halvings, then point edits) so predicates that
//! invoke jq converge in few rechecks. The driver alternates input and
//! program passes to a joint fixpoint under a recheck budget.

use tjq_exec::{Filter, Json};

/// Greedily shrink a failing (program, input) pair. `still_fails` is
/// consulted at most `budget` times; the current pair always fails.
pub fn shrink_case<P>(
    program: &Filter,
    input: &Json,
    mut still_fails: P,
    budget: usize,
) -> (Filter, Json)
where
    P: FnMut(&Filter, &Json) -> bool,
{
    let mut cur_p = program.clone();
    let mut cur_i = input.clone();
    let mut checks = 0usize;

    loop {
        let mut progressed = false;

        // Input pass
        loop {
            let mut stepped = false;
            for cand in shrink_json(&cur_i) {
                if checks >= budget {
                    return (cur_p, cur_i);
                }
                checks += 1;
                if still_fails(&cur_p, &cand) {
                    cur_i = cand;
                    stepped = true;
                    progressed = true;
                    break;
                }
            }
            if !stepped {
                break;
            }
        }

        // Program pass
        loop {
            let mut stepped = false;
            for cand in shrink_filter(&cur_p) {
                if checks >= budget {
                    return (cur_p, cur_i);
                }
                checks += 1;
                if still_fails(&cand, &cur_i) {
                    cur_p = cand;
                    stepped = true;
                    progressed = true;
                    break;
                }
            }
            if !stepped {
                break;
            }
        }

        if !progressed {
            return (cur_p, cur_i);
        }
    }
}

/// One-step reductions of a JSON value, aggressive first.
pub fn shrink_json(j: &Json) -> Vec<Json> {
    let mut out = vec![];

    // Whole-value replacements
    if !matches!(j, Json::Null) {
        out.push(Json::Null);
    }
    // Hoist children: the failure often lives in one element
    match j {
        Json::Array(arr) => {
            for child in arr.iter().take(4) {
                out.push(child.clone());
            }
        }
        Json::Object(fields) => {
            for (_, v) in fields.iter().take(4) {
                out.push(v.clone());
            }
        }
        _ => {}
    }
    match j {
        Json::Number(n) => {
            if *n != 0.0 {
                out.push(Json::Number(0.0));
            }
            let t = n.trunc();
            if t != *n {
                out.push(Json::Number(t));
            }
            if n.abs() >= 2.0 {
                out.push(Json::Number(n / 2.0));
            }
            if *n < 0.0 {
                out.push(Json::Number(-n));
            }
        }
        Json::String(s) => {
            if !s.is_empty() {
                out.push(Json::String(String::new()));
                let chars: Vec<char> = s.chars().collect();
                if chars.len() > 1 {
                    out.push(Json::String(chars[..chars.len() / 2].iter().collect()));
                    out.push(Json::String(chars[chars.len() / 2..].iter().collect()));
                }
                if s.chars().any(|c| c != 'a') {
                    out.push(Json::String("a".repeat(chars.len().min(3))));
                }
            }
        }
        Json::Boolean(true) => out.push(Json::Boolean(false)),
        Json::Boolean(false) => {}
        Json::Array(arr) => {
            if !arr.is_empty() {
                out.push(Json::Array(vec![]));
                let n = arr.len();
                if n > 1 {
                    out.push(Json::Array(arr[..n / 2].to_vec()));
                    out.push(Json::Array(arr[n / 2..].to_vec()));
                }
                // Per-element removal and in-place shrinking, bounded for
                // wide arrays (halvings above handle the bulk)
                if n <= 16 {
                    for i in 0..n {
                        let mut smaller = arr.clone();
                        smaller.remove(i);
                        out.push(Json::Array(smaller));
                    }
                    for i in 0..n {
                        for cand in shrink_json(&arr[i]) {
                            let mut replaced = arr.clone();
                            replaced[i] = cand;
                            out.push(Json::Array(replaced));
                        }
                    }
                } else {
                    // Hoist a single element as a candidate witness
                    out.push(Json::Array(vec![arr[0].clone()]));
                }
            }
        }
        Json::Object(fields) => {
            if !fields.is_empty() {
                out.push(Json::Object(vec![]));
                let n = fields.len();
                if n > 1 {
                    out.push(Json::Object(fields[..n / 2].to_vec()));
                    out.push(Json::Object(fields[n / 2..].to_vec()));
                }
                if n <= 16 {
                    for i in 0..n {
                        let mut smaller = fields.clone();
                        smaller.remove(i);
                        out.push(Json::Object(smaller));
                    }
                    for i in 0..n {
                        for cand in shrink_json(&fields[i].1) {
                            let mut replaced = fields.clone();
                            replaced[i] = (replaced[i].0.clone(), cand);
                            out.push(Json::Object(replaced));
                        }
                    }
                }
            }
        }
        Json::Null => {}
    }

    out
}

/// One-step reductions of a program, aggressive first: replace by a leaf,
/// hoist a subterm to the root, drop container elements, then recurse.
pub fn shrink_filter(f: &Filter) -> Vec<Filter> {
    let mut out = vec![];

    // Whole-program replacements
    if !matches!(f, Filter::Dot) {
        out.push(Filter::Dot);
    }
    if !matches!(f, Filter::Null | Filter::Dot) {
        out.push(Filter::Null);
    }

    // Hoist direct subterms to the root
    match f {
        Filter::Pipe(a, b) | Filter::Comma(a, b) => {
            out.push(a.as_ref().clone());
            out.push(b.as_ref().clone());
        }
        Filter::BinOp(l, _, r) => {
            out.push(l.as_ref().clone());
            out.push(r.as_ref().clone());
        }
        Filter::UnOp(_, inner) => out.push(inner.as_ref().clone()),
        Filter::IfThenElse(c, t, e) => {
            out.push(t.as_ref().clone());
            out.push(e.as_ref().clone());
            out.push(c.as_ref().clone());
        }
        Filter::Array(items) => {
            for item in items {
                out.push(item.clone());
            }
            for i in 0..items.len() {
                let mut smaller = items.clone();
                smaller.remove(i);
                out.push(Filter::Array(smaller));
            }
        }
        Filter::Object(fields) => {
            for (_, v) in fields {
                out.push(v.clone());
            }
            for i in 0..fields.len() {
                let mut smaller = fields.clone();
                smaller.remove(i);
                out.push(Filter::Object(smaller));
            }
        }
        _ => {}
    }

    // Recurse: rebuild with one shrunk child
    match f {
        Filter::Pipe(a, b) => {
            for cand in shrink_filter(a) {
                out.push(Filter::Pipe(Box::new(cand), b.clone()));
            }
            for cand in shrink_filter(b) {
                out.push(Filter::Pipe(a.clone(), Box::new(cand)));
            }
        }
        Filter::Comma(a, b) => {
            for cand in shrink_filter(a) {
                out.push(Filter::Comma(Box::new(cand), b.clone()));
            }
            for cand in shrink_filter(b) {
                out.push(Filter::Comma(a.clone(), Box::new(cand)));
            }
        }
        Filter::BinOp(l, op, r) => {
            for cand in shrink_filter(l) {
                out.push(Filter::BinOp(Box::new(cand), *op, r.clone()));
            }
            for cand in shrink_filter(r) {
                out.push(Filter::BinOp(l.clone(), *op, Box::new(cand)));
            }
        }
        Filter::UnOp(op, inner) => {
            for cand in shrink_filter(inner) {
                out.push(Filter::UnOp(*op, Box::new(cand)));
            }
        }
        Filter::IfThenElse(c, t, e) => {
            for cand in shrink_filter(c) {
                out.push(Filter::IfThenElse(Box::new(cand), t.clone(), e.clone()));
            }
            for cand in shrink_filter(t) {
                out.push(Filter::IfThenElse(c.clone(), Box::new(cand), e.clone()));
            }
            for cand in shrink_filter(e) {
                out.push(Filter::IfThenElse(c.clone(), t.clone(), Box::new(cand)));
            }
        }
        Filter::Array(items) => {
            for i in 0..items.len() {
                for cand in shrink_filter(&items[i]) {
                    let mut replaced = items.clone();
                    replaced[i] = cand;
                    out.push(Filter::Array(replaced));
                }
            }
        }
        Filter::Object(fields) => {
            for i in 0..fields.len() {
                for cand in shrink_filter(&fields[i].1) {
                    let mut replaced = fields.clone();
                    replaced[i] = (replaced[i].0.clone(), cand);
                    out.push(Filter::Object(replaced));
                }
            }
        }
        // Literal simplifications
        Filter::Number(n) if *n != 0.0 => out.push(Filter::Number(0.0)),
        Filter::String(s) if !s.is_empty() => out.push(Filter::String(String::new())),
        Filter::Boolean(true) => out.push(Filter::Boolean(false)),
        _ => {}
    }

    out
}
