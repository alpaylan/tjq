//! A bytecode compiler and backtracking VM for a core subset of `Filter`.
//!
//! jq filters are stream (generator) transformers, so the VM is a backtracking
//! stack machine: a value threads through as the top of an operand stack, and
//! `Fork` instructions push choice points that are resumed (restoring the
//! stack) to produce a filter's successive outputs. Operator semantics are
//! shared with the tree-walking interpreter (`apply_binop`) so the two engines
//! cannot diverge; the whole point is to differentially test this VM against
//! the interpreter and, later, benchmark it against jq's own bytecode engine.
//!
//! The supported subset (see `compile`) covers straight-line generator control
//! flow — identity, literals, pipe, comma, indexing, iteration, arithmetic and
//! comparison, negation, and if/then/else. Array/object construction, bindings,
//! reduce/foreach, and calls are not yet compiled; `compile` returns
//! `Err(Unsupported)` for them, and the differential harness skips those.

use crate::error::JQError;
use crate::filter::{apply_binop, Filter};
use crate::json::Json;
use crate::{BinOp, UnOp};

/// A single bytecode instruction. Jump targets are absolute indices into the
/// instruction vector.
#[derive(Debug, Clone, PartialEq)]
pub enum Inst {
    /// Identity: leave the current value untouched.
    Dot,
    /// Replace the current value with a constant (a literal ignores its input).
    Push(Json),
    /// Duplicate the current value (push a clone of the top).
    Dup,
    /// Swap the top two stack values.
    Swap,
    /// Index the current value by a constant string key (`.foo`).
    IndexField(String),
    /// Generic index `.[expr]`: pop the index, then the container, push
    /// `container[index]` (array+int, object+string, null-lenient).
    IndexGeneric,
    /// Iterate `.[]`: fork over the current value's elements/values.
    Iterate,
    /// Pop right then left, push `apply_binop(left, right, op)`.
    Binop(BinOp),
    /// Unary negation of the current value.
    Neg,
    /// Push a choice point resuming at `target` with the stack restored.
    Fork(usize),
    /// Unconditional jump.
    Jump(usize),
    /// Pop a value; if it is falsy (null/false), jump to `target`.
    JumpIfNot(usize),
    /// Pop a value; if it is truthy, jump to `target`.
    JumpIf(usize),
    /// Replace the current value with its boolean truthiness.
    ToBool,
    /// Produce nothing for this path (`empty`): backtrack immediately.
    Backtrack,
    /// Raise an error for this path (`error`).
    Error,
}

/// A filter that the core compiler does not yet handle.
#[derive(Debug, Clone)]
pub struct Unsupported;

/// Compile a filter into a flat instruction vector, or report that it uses a
/// construct outside the supported core.
pub fn compile(f: &Filter) -> Result<Vec<Inst>, Unsupported> {
    let mut code = Vec::new();
    emit(f, &mut code)?;
    Ok(code)
}

fn emit(f: &Filter, code: &mut Vec<Inst>) -> Result<(), Unsupported> {
    match f {
        Filter::Dot => code.push(Inst::Dot),
        Filter::Null => code.push(Inst::Push(Json::Null)),
        Filter::Boolean(b) => code.push(Inst::Push(Json::Boolean(*b))),
        Filter::Number(n) => code.push(Inst::Push(Json::Number(*n))),
        Filter::String(s) => code.push(Inst::Push(Json::String(s.clone()))),
        Filter::Empty => code.push(Inst::Backtrack),
        Filter::Error => code.push(Inst::Error),
        Filter::Pipe(a, b) => {
            emit(a, code)?;
            emit(b, code)?;
        }
        Filter::Comma(a, b) => {
            // Fork(Lb); <a>; Jump(Lend); Lb: <b>; Lend:
            let fork_at = code.len();
            code.push(Inst::Fork(0)); // patched
            emit(a, code)?;
            let jump_at = code.len();
            code.push(Inst::Jump(0)); // patched
            let lb = code.len();
            code[fork_at] = Inst::Fork(lb);
            emit(b, code)?;
            let lend = code.len();
            code[jump_at] = Inst::Jump(lend);
        }
        Filter::ObjIndex(inner) => match inner.as_ref() {
            Filter::String(s) => code.push(Inst::IndexField(s.clone())),
            _ => return Err(Unsupported),
        },
        Filter::ArrayIndex(idx) => {
            // `.[expr]`: the index expression runs on the input (the container).
            code.push(Inst::Dup);
            emit(idx, code)?;
            code.push(Inst::IndexGeneric);
        }
        Filter::ArrayIterator => code.push(Inst::Iterate),
        Filter::UnOp(UnOp::Neg, inner) => {
            emit(inner, code)?;
            code.push(Inst::Neg);
        }
        // `and`/`or` short-circuit and boolify both operands (they are *not*
        // cartesian binops): a truthy `or` lhs (falsy `and` lhs) yields the
        // boolean without evaluating the rhs. The lhs is a generator, so each
        // of its values decides independently.
        Filter::BinOp(l, BinOp::Or, r) => {
            code.push(Inst::Dup);
            emit(l, code)?;
            let jin = code.len();
            code.push(Inst::JumpIfNot(0)); // lhs falsy -> evaluate rhs
            code.push(Inst::Push(Json::Boolean(true))); // lhs truthy -> true
            let jmp = code.len();
            code.push(Inst::Jump(0));
            let leval = code.len();
            code[jin] = Inst::JumpIfNot(leval);
            emit(r, code)?;
            code.push(Inst::ToBool);
            let lend = code.len();
            code[jmp] = Inst::Jump(lend);
        }
        Filter::BinOp(l, BinOp::And, r) => {
            code.push(Inst::Dup);
            emit(l, code)?;
            let jif = code.len();
            code.push(Inst::JumpIf(0)); // lhs truthy -> evaluate rhs
            code.push(Inst::Push(Json::Boolean(false))); // lhs falsy -> false
            let jmp = code.len();
            code.push(Inst::Jump(0));
            let leval = code.len();
            code[jif] = Inst::JumpIf(leval);
            emit(r, code)?;
            code.push(Inst::ToBool);
            let lend = code.len();
            code[jmp] = Inst::Jump(lend);
        }
        Filter::BinOp(l, op, r) => {
            // jq iterates the right operand in the outer loop and the left in
            // the inner one, so emit the right first (making the left backtrack
            // innermost). The stack ends as [right_val, left_val]; `Binop` pops
            // the left (top) then the right.
            code.push(Inst::Dup);
            emit(r, code)?;
            code.push(Inst::Swap);
            emit(l, code)?;
            code.push(Inst::Binop(*op));
        }
        Filter::IfThenElse(c, t, e) => {
            // Dup; <c>; JumpIfNot(Le); <t>; Jump(Lend); Le: <e>; Lend:
            code.push(Inst::Dup);
            emit(c, code)?;
            let jin_at = code.len();
            code.push(Inst::JumpIfNot(0)); // patched
            emit(t, code)?;
            let jump_at = code.len();
            code.push(Inst::Jump(0)); // patched
            let le = code.len();
            code[jin_at] = Inst::JumpIfNot(le);
            emit(e, code)?;
            let lend = code.len();
            code[jump_at] = Inst::Jump(lend);
        }
        _ => return Err(Unsupported),
    }
    Ok(())
}

/// A choice point. `Branch` resumes at a pc; `Iter` resumes iteration over a
/// container's remaining elements. Both restore the operand stack snapshot.
enum Fork {
    Branch {
        pc: usize,
        stack: Vec<Json>,
    },
    Iter {
        pc: usize,
        stack: Vec<Json>,
        items: Vec<Json>,
        idx: usize,
    },
}

/// Bounds VM steps so a pathological program cannot loop forever; on overflow
/// the run ends with an allocation-guard error (mirrors the interpreter).
const STEP_BUDGET: usize = 50_000_000;

/// Run compiled `code` on `input`, returning the output stream (values and
/// errors interleaved, in generation order) — the same shape the tree-walking
/// interpreter produces.
pub fn run(code: &[Inst], input: Json) -> Vec<Result<Json, JQError>> {
    let mut out: Vec<Result<Json, JQError>> = Vec::new();
    let mut stack: Vec<Json> = vec![input];
    let mut forks: Vec<Fork> = Vec::new();
    let mut pc = 0usize;
    let mut steps = 0usize;

    // Resume the most recent choice point; returns false when none remain.
    fn backtrack(forks: &mut Vec<Fork>, stack: &mut Vec<Json>, pc: &mut usize) -> bool {
        while let Some(f) = forks.last_mut() {
            match f {
                Fork::Branch { pc: fpc, stack: fstack } => {
                    *stack = std::mem::take(fstack);
                    *pc = *fpc;
                    forks.pop();
                    return true;
                }
                Fork::Iter { pc: fpc, stack: fstack, items, idx } => {
                    if *idx < items.len() {
                        *stack = fstack.clone();
                        stack.push(items[*idx].clone());
                        *pc = *fpc;
                        *idx += 1;
                        return true;
                    }
                    forks.pop();
                }
            }
        }
        false
    }

    loop {
        steps += 1;
        if steps > STEP_BUDGET {
            out.push(Err(JQError::AllocationTooLarge));
            return out;
        }
        if pc >= code.len() {
            // End of a path: emit the current value, then backtrack.
            out.push(Ok(stack.last().cloned().unwrap_or(Json::Null)));
            if !backtrack(&mut forks, &mut stack, &mut pc) {
                return out;
            }
            continue;
        }
        match &code[pc] {
            Inst::Dot => pc += 1,
            Inst::Push(v) => {
                if let Some(top) = stack.last_mut() {
                    *top = v.clone();
                } else {
                    stack.push(v.clone());
                }
                pc += 1;
            }
            Inst::Dup => {
                let t = stack.last().cloned().unwrap_or(Json::Null);
                stack.push(t);
                pc += 1;
            }
            Inst::Swap => {
                let n = stack.len();
                stack.swap(n - 1, n - 2);
                pc += 1;
            }
            Inst::IndexField(key) => {
                let top = stack.pop().unwrap_or(Json::Null);
                match index_value(&top, &Json::String(key.clone())) {
                    Ok(v) => {
                        stack.push(v);
                        pc += 1;
                    }
                    Err(e) => {
                        out.push(Err(e));
                        if !backtrack(&mut forks, &mut stack, &mut pc) {
                            return out;
                        }
                    }
                }
            }
            Inst::IndexGeneric => {
                let idx = stack.pop().unwrap_or(Json::Null);
                let container = stack.pop().unwrap_or(Json::Null);
                match index_value(&container, &idx) {
                    Ok(v) => {
                        stack.push(v);
                        pc += 1;
                    }
                    Err(e) => {
                        out.push(Err(e));
                        if !backtrack(&mut forks, &mut stack, &mut pc) {
                            return out;
                        }
                    }
                }
            }
            Inst::Iterate => {
                let container = stack.pop().unwrap_or(Json::Null);
                let items: Option<Vec<Json>> = match container {
                    Json::Array(a) => Some(a),
                    Json::Object(o) => Some(o.into_iter().map(|(_, v)| v).collect()),
                    other => {
                        out.push(Err(JQError::ArrIteratorForNonIterable(other)));
                        None
                    }
                };
                match items {
                    None => {
                        if !backtrack(&mut forks, &mut stack, &mut pc) {
                            return out;
                        }
                    }
                    Some(items) if items.is_empty() => {
                        if !backtrack(&mut forks, &mut stack, &mut pc) {
                            return out;
                        }
                    }
                    Some(items) => {
                        // Push the first element now; a fork yields the rest.
                        stack.push(items[0].clone());
                        forks.push(Fork::Iter {
                            pc: pc + 1,
                            stack: stack[..stack.len() - 1].to_vec(),
                            items,
                            idx: 1,
                        });
                        pc += 1;
                    }
                }
            }
            Inst::Binop(op) => {
                // Stack is [right_val, left_val]; the left is on top.
                let l = stack.pop().unwrap_or(Json::Null);
                let r = stack.pop().unwrap_or(Json::Null);
                match apply_binop(l, r, *op) {
                    Ok(v) => {
                        stack.push(v);
                        pc += 1;
                    }
                    Err(e) => {
                        out.push(Err(e));
                        if !backtrack(&mut forks, &mut stack, &mut pc) {
                            return out;
                        }
                    }
                }
            }
            Inst::Neg => {
                let top = stack.pop().unwrap_or(Json::Null);
                match top {
                    Json::Number(n) => {
                        stack.push(Json::Number(-n));
                        pc += 1;
                    }
                    other => {
                        out.push(Err(JQError::UnOpTypeError(other, UnOp::Neg)));
                        if !backtrack(&mut forks, &mut stack, &mut pc) {
                            return out;
                        }
                    }
                }
            }
            Inst::Fork(target) => {
                forks.push(Fork::Branch {
                    pc: *target,
                    stack: stack.clone(),
                });
                pc += 1;
            }
            Inst::Jump(target) => pc = *target,
            Inst::JumpIfNot(target) => {
                let cond = stack.pop().unwrap_or(Json::Null);
                if cond.boolify() {
                    pc += 1;
                } else {
                    pc = *target;
                }
            }
            Inst::JumpIf(target) => {
                let cond = stack.pop().unwrap_or(Json::Null);
                if cond.boolify() {
                    pc = *target;
                } else {
                    pc += 1;
                }
            }
            Inst::ToBool => {
                let top = stack.pop().unwrap_or(Json::Null);
                stack.push(Json::Boolean(top.boolify()));
                pc += 1;
            }
            Inst::Backtrack => {
                if !backtrack(&mut forks, &mut stack, &mut pc) {
                    return out;
                }
            }
            Inst::Error => {
                out.push(Err(JQError::Unknown));
                if !backtrack(&mut forks, &mut stack, &mut pc) {
                    return out;
                }
            }
        }
    }
}

/// jq's generic index, mirroring the tree interpreter: arrays take integer
/// indices (negatives count from the end), objects take string keys, null
/// indexes to null, and everything else is a type error.
fn index_value(container: &Json, idx: &Json) -> Result<Json, JQError> {
    match (container, idx) {
        (Json::Array(arr), Json::Number(n)) => {
            if n.is_nan() || n.is_infinite() || n.fract() != 0.0 {
                return Err(JQError::InvalidArrayIndex(
                    container.clone(),
                    Json::Number(*n),
                ));
            }
            let len = arr.len() as i64;
            let mut k = *n as i64;
            if k < 0 {
                k += len;
            }
            if k < 0 || k >= len {
                Ok(Json::Null)
            } else {
                Ok(arr[k as usize].clone())
            }
        }
        (Json::Object(obj), Json::String(key)) => Ok(obj
            .iter()
            .find(|(k, _)| k == key)
            .map(|(_, v)| v.clone())
            .unwrap_or(Json::Null)),
        (Json::Null, Json::Number(_)) | (Json::Null, Json::String(_)) => Ok(Json::Null),
        (Json::Array(_), other) => Err(JQError::InvalidArrayIndex(container.clone(), other.clone())),
        (Json::Object(_), other) => Err(JQError::NonStringObjectKey(other.clone())),
        _ => Err(JQError::ArrIndexForNonArray(container.clone())),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse;

    fn parse_filter(src: &str) -> Filter {
        let (_defs, cst) = parse(src);
        (&cst).into()
    }

    fn json(s: &str) -> Json {
        let v: serde_json::Value = serde_json::from_str(s).unwrap();
        fn conv(v: &serde_json::Value) -> Json {
            match v {
                serde_json::Value::Null => Json::Null,
                serde_json::Value::Bool(b) => Json::Boolean(*b),
                serde_json::Value::Number(n) => Json::Number(n.as_f64().unwrap()),
                serde_json::Value::String(s) => Json::String(s.clone()),
                serde_json::Value::Array(a) => Json::Array(a.iter().map(conv).collect()),
                serde_json::Value::Object(o) => {
                    Json::Object(o.iter().map(|(k, v)| (k.clone(), conv(v))).collect())
                }
            }
        }
        conv(&v)
    }

    fn eval(src: &str, input: &str) -> Vec<Json> {
        let code = compile(&parse_filter(src)).expect("supported");
        run(&code, json(input))
            .into_iter()
            .map(|r| r.expect("no error"))
            .collect()
    }

    #[test]
    fn vm_core() {
        assert_eq!(eval(".", "5"), vec![json("5")]);
        assert_eq!(eval("1 + 2", "null"), vec![json("3")]);
        assert_eq!(eval(".a", "{\"a\":7}"), vec![json("7")]);
        assert_eq!(eval(".[]", "[1,2,3]"), vec![json("1"), json("2"), json("3")]);
        assert_eq!(eval(".[] | . + 1", "[1,2]"), vec![json("2"), json("3")]);
        assert_eq!(eval("1, 2, 3", "null"), vec![json("1"), json("2"), json("3")]);
        assert_eq!(eval(".[1]", "[10,20,30]"), vec![json("20")]);
        assert_eq!(eval(".[-1]", "[10,20,30]"), vec![json("30")]);
        assert_eq!(eval("-.", "5"), vec![json("-5")]);
        assert_eq!(
            eval("if . > 1 then \"big\" else \"small\" end", "2"),
            vec![json("\"big\"")]
        );
    }

    #[test]
    fn vm_cartesian() {
        // Binop over two streams is a cartesian product.
        assert_eq!(
            eval("(1,2) + (10,20)", "null"),
            vec![json("11"), json("12"), json("21"), json("22")]
        );
    }
}
