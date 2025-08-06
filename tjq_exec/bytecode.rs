use std::sync::Arc;

use crate::filter::Filter;
use crate::json::Json;

#[derive(Debug, Clone)]
enum Opcode {
    Nop,
    Push,
    Pop,
    Dup,
    Const,
    Load,
    Store,
    Object,
    Append,
    Fork,
    ForkTryBegin,
    ForkTryEnd,
    ForkAlt,
    ForkLabel,
    Backtrack,
    Jump,
    JumpIfNot,
    Index,
    IndexArray,
    Call,
    CallRec,
    PushPc,
    CallPc,
    Scope,
    Ret,
    Iter,
    ExpBegin,
    ExpEnd,
    PathBegin,
    PathEnd,
}

#[derive(Debug, Clone)]
struct Program {
    vars: Vec<String>,
    codes: Vec<Arc<Code>>,
    codeinfos: Vec<CodeInfo>,
}

#[derive(Debug, Clone)]
struct Code {
    v: Box<Json>,
    op: Opcode,
}

#[derive(Debug, Clone)]
struct CodeInfo {
    name: String,
    pc: i32,
}

struct Env {
    pc: usize,
    stack: Stack,
    paths: Stack,
    scopes: ScopeStack,
    values: Vec<Box<Json>>,
    codes: Vec<Arc<Code>>,
    codeinfos: Vec<CodeInfo>,
    forks: Vec<Fork>,
    backtrack: bool,
    offset: usize,
    expdepth: usize,
    label: usize,
    args: [Box<Json>; 32], // len(env.args) > maxarity
                           // ctx: Context, // Add your context type here if needed
}

impl Env {
    // Executes the bytecode with the given initial value and variables.
    // Returns an iterator-like Env for stepping through results.
    fn execute(
        &mut self,
        codes: Vec<Arc<Code>>,
        codeinfos: Vec<CodeInfo>,
        v: Box<Json>,
        vars: &[Box<Json>],
    ) {
        self.codes = codes;
        self.codeinfos = codeinfos;
        self.stack.push(v);
        for var in vars.iter().rev() {
            self.stack.push(var.clone());
        }
        // self.debug_codes(); // Implement if needed
        // This function sets up the environment for execution.
    }

    // Steps through the bytecode, returning the next result and a boolean indicating completion.
    fn next(&mut self) -> (Option<Box<Json>>, bool) {
        todo!()
    }
}

#[derive(Debug, Clone)]
struct Scope {
    id: i32,
    offset: i32,
    pc: i32,
    saveindex: i32,
    outerindex: i32,
}

#[derive(Debug, Clone)]
struct Fork {
    pc: i32,
    stackindex: i32,
    stacklimit: i32,
    scopeindex: i32,
    scopelimit: i32,
    pathindex: i32,
    pathlimit: i32,
    expdepth: i32,
}

#[derive(Debug, Clone)]
struct ScopeBlock {
    value: Scope,
    next: isize,
}

#[derive(Debug, Clone)]
struct ScopeStack {
    data: Vec<ScopeBlock>,
    index: isize,
    limit: isize,
}

impl ScopeStack {
    fn new() -> Self {
        ScopeStack {
            data: Vec::new(),
            index: -1,
            limit: -1,
        }
    }

    fn push(&mut self, v: Scope) {
        let b = ScopeBlock {
            value: v,
            next: self.index,
        };
        self.index = std::cmp::max(self.index, self.limit) + 1;
        if (self.index as usize) < self.data.len() {
            self.data[self.index as usize] = b;
        } else {
            self.data.push(b);
        }
    }

    fn pop(&mut self) -> Option<Scope> {
        if self.index < 0 {
            return None;
        }
        let b = &self.data[self.index as usize];
        self.index = b.next;
        Some(b.value.clone())
    }

    fn empty(&self) -> bool {
        self.index < 0
    }

    fn save(&mut self) -> (isize, isize) {
        let index = self.index;
        let limit = self.limit;
        if self.index > self.limit {
            self.limit = self.index;
        }
        (index, limit)
    }

    fn restore(&mut self, index: isize, limit: isize) {
        self.index = index;
        self.limit = limit;
    }
}

#[derive(Debug, Clone)]
struct Block {
    value: Box<Json>,
    next: isize,
}

#[derive(Debug, Clone)]
struct Stack {
    data: Vec<Block>,
    index: isize,
    limit: isize,
}

impl Stack {
    fn new() -> Self {
        Stack {
            data: Vec::new(),
            index: -1,
            limit: -1,
        }
    }

    fn push(&mut self, v: Box<Json>) {
        let b = Block {
            value: v,
            next: self.index,
        };
        self.index = std::cmp::max(self.index, self.limit) + 1;
        if (self.index as usize) < self.data.len() {
            self.data[self.index as usize] = b;
        } else {
            self.data.push(b);
        }
    }

    fn pop(&mut self) -> Option<Box<Json>> {
        if self.index < 0 {
            return None;
        }
        let b = &self.data[self.index as usize];
        self.index = b.next;
        Some(b.value.clone())
    }

    fn empty(&self) -> bool {
        self.index < 0
    }

    fn save(&mut self) -> (isize, isize) {
        let index = self.index;
        let limit = self.limit;
        if self.index > self.limit {
            self.limit = self.index;
        }
        (index, limit)
    }

    fn restore(&mut self, index: isize, limit: isize) {
        self.index = index;
        self.limit = limit;
    }
}

struct Compiler {
    // module_loader: ModuleLoader, // Define ModuleLoader if needed
    // environ_loader: fn() -> Vec<String>, // Uncomment and define if needed
    variables: Vec<String>,
    // custom_funcs: HashMap<String, Function>, // Define Function if needed
    // input_iter: Iter, // Define Iter if needed
    codes: Vec<Arc<Code>>,
    codeinfos: Vec<CodeInfo>,
    // builtin_scope: Option<ScopeInfo>, // Define ScopeInfo if needed
    // scopes: Vec<ScopeInfo>, // Define ScopeInfo if needed
    scopecnt: usize,
}

impl Compiler {
    fn new() -> Self {
        Compiler {
            // module_loader: ModuleLoader::new(), // Uncomment if needed
            variables: Vec::new(),
            codes: Vec::new(),
            codeinfos: Vec::new(),
            scopecnt: 0,
        }
    }

    pub fn compile(&mut self, f: Filter) -> Program {
        // Implement the compilation logic here
        match f {
            Filter::Dot => {
                        todo!()
                    }
            Filter::Pipe(filter, filter1) => todo!(),
            Filter::Comma(filter, filter1) => todo!(),
            Filter::ObjIndex(_) => todo!(),
            Filter::ArrayIndex(_) => todo!(),
            Filter::ArrayIterator => todo!(),
            Filter::Null => todo!(),
            Filter::Boolean(_) => todo!(),
            Filter::Number(_) => todo!(),
            Filter::String(_) => todo!(),
            Filter::Array(filters) => todo!(),
            Filter::Object(items) => todo!(),
            Filter::UnOp(un_op, filter) => todo!(),
            Filter::BinOp(filter, bin_op, filter1) => todo!(),
            Filter::Empty => todo!(),
            Filter::Error => todo!(),
            Filter::Call(_, filters) => todo!(),
            Filter::IfThenElse(filter, filter1, filter2) => todo!(),
            Filter::Bound(items, filter) => todo!(),
            Filter::FunctionExpression(hash_map, filter) => todo!(),
            Filter::BindingExpression(filter, filter1) => todo!(),
            Filter::Variable(_) => todo!(),
            Filter::ReduceExpression(hash_map, filter, filter1) => todo!(),
Filter::Hole => todo!(),
        }
    }
}

impl Program {
    fn run(&self, vars: Vec<Json>) -> impl Iterator<Item = Json> {
        vec![].into_iter()
    }
}

#[cfg(test)]
mod tests {
    

    // #[test]
    // fn example_compile_dot() {
    //     let query = Filter::Dot;
    //     let mut compiler = Compiler::new();
    //     let code = compiler.compile(query);

    //     let result = code.run(vec![
    //         Json::Null,
    //         "string".into(),
    //         42.into(),
    //         vec!["foo"].into(),
    //         vec![("foo", 42)].into(),
    //     ]);
    //     for v in result {
    //         println!("Result: {:?}", v);
    //     }
    // }

    // #[test]
    // fn example_compile() {
    //     let query = Filter::Pipe(
    //         Box::new(Filter::ArrayIterator),
    //         Box::new(Filter::ObjIndex("foo".into())),
    //     );
    //     let mut compiler = Compiler::new();
    //     let code = compiler.compile(query);

    //     let result = code.run(vec![
    //         Json::Null,
    //         "string".into(),
    //         42.into(),
    //         vec!["foo"].into(),
    //         vec![("foo", 42)].into(),
    //     ]);
    //     for v in result {
    //         println!("Result: {:?}", v);
    //     }
    // }
}
