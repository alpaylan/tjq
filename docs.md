# Typed JQ

JQ is a successful tool for json manipulation.

```bash
$ jq -nc '[1, 2, 3] | map(select(. % 2 == 1))'
> [1, 3]
```

A major shortcoming of jq, often mentioned by practitioners, is its error messages and debugging
capabilities. A jq program typically takes a large set of json inputs, transforms and processes
these inputs through a set of filters(programs), and produces a set of resulting values. jq interpreter
does not keep track of the dataflow through the program, resulting in **local errors**. Such local errors
give us a micro picture of the problem, but fails to inform us of the *why*. Below is a simple demonstration.

```bash
$  jq -nc '[{"name": "John", "age": 25}, {"name": "Jane", "age": 30}] | .[] | .age, .name | {v: .a}'
> jq: error (at <unknown>): Cannot index number with string "a"
```

The local error `Cannot index number with string "a"` makes perfect sense, one indeed cannot index a number with
a string. The more important questions, which are (1) where do we index an input with string "a", and (2) where does
the number come from, are left unanswered. While (1) is a property of the program, (2) is a property of the input.

Let's for a second, try to informally answer the posed questions. What would be the nice informative response useful
to the user?

> The program traverses the given array, accesses its age and name fields, and constructs a json object by accessing
> "a" field of the ages and names. As `25` is a number, not an object, it has no field "a", hence the program fails.

Spoiler alert, our type inference procedure will give us something very close to this textual explanation of the error:

```text
Shape mismatch detected!
        at [0].age
        Expected: {a: <>}
        Got: 25
```

We can produce such informative error messages by statically analyzing the jq programs, inferring the shape of the
JSON inputs a program accepts and comparing the inferred shape with the real inputs. An inferred shape is similar to
a structurally typed record, below is the inferred shape for the provided example, and a typescript type that matches
the same structure.

```text
[
    {
        age: {a: <>}, 
        name: {a: <>}
    }
]
```

The `<>` represents an unconstrained JSON term. Below is the corresponding typescript type definition.

```typescript
type X = {
    age: any
    name: any
}[];
```

## Formalization

For the purposes of prototyping, we have started with a small subset of jq. Below is the definition of the tjq AST in Rust.

```rust
enum Filter {
    Dot,                             // .
    Pipe(Box<Filter>, Box<Filter>),  // <f_1> | <f_2>
    Comma(Box<Filter>, Box<Filter>), // <f_1>, <f_2>
    ObjIndex(String),                // .<s>
    ArrayIndex(usize),               // .[<n>]
    ArrayIterator,                   // .[]
    Null,                            // null
    Boolean(bool),                   // true | false
    Number(f64),                     // 1, 2..
    String(String),                  // "abc"
    Array(Vec<Filter>),              // [...]
    Object(Vec<(Filter, Filter)>),   // {...}
}
```

The language contains a duplicate definition of JSON inside, because we can construct JSON terms from the prior
results from the program.

Over this subset language, we have implemented an interpreter, and written constraint rules for every construct of the language, which we
have implemented in Rust as part of the analysis.

Given an input such as `[{name: John, age: 25}, {name: Jane, age: 30}]`, we can currently interpret the filter `.[] | .age, .name` to give the result

```text
25
John
30
Jane
```

The constraint rules are akin to symbolic execution. We start with an unconstrainted symbolic input `<>`, and execute the program with this input,
using the constraint rules to refine the symbolic input further and further.

Let's see how that works.  Below, we follow through the constraint generation for  `.[] | .a, .b`:

1. `<> :: .[] --> .[]` 
2. `C :: .a,.b --> (C :: .a) + (C :: .b)`
   1. `.[] :: .a --> .[].a`
   2. `.[] :: .a --> .[].b`
   3. `.[] :: .a,.b --> .[].a + .[].b`
3. `.[].a + .[].b ==> [{"a": <>, "b": <>}]`

While steps (1) and (2) are part of the constraint generation, step (3) is the next step, we build a shape from the constraint.
Below, we write the rules for each construct:

### Rules

In the rules, `C` represents the state of the symbolic input before the execution of the current construct.

#### Dot `.` rule

`C :: . --> C`

As `.` filter does nothing to the input, but merely passes it forward, it does not affect the constraints.

#### Pipe `|` rule

```text
C :: f1 --> X       X :: f2 --> Y
---------------------------------
        C :: f1 | f2 --> Y
```

A pipe is similar to the sequential execution in an imperative program. The execution of the left-hand-side of the pipe
changes the constraint(program state), which is used by the right-hand-side.

#### Comma `,` rule

```text
C :: f1 --> X       C :: f2 --> Y
---------------------------------
        C :: f1, f2 --> X + Y
```

A comma deduplicates the current constraint, and merges the resulting constraints together.

#### Value `json` rules

An integer, boolean, string or null produces no constraints

```text
C :: i --> <>
C :: b --> <>
C :: s --> <>
C :: null --> <>
```

An array or object produces the sum of the constraints for their elements, similar to the `comma` rule.

##### Array `[]` rule

```text
    C :: v_1 --> X_1    C :: v_2 --> X_2   ...   C :: v_n --> X_n
---------------------------------------------------------------------
          C :: [ v_1 , v_2 ... v_n ] --> X_1, X_2...X_n
```

##### Object `{}` rule

```text
C :: k_1 --> X_1       C :: v_1 --> Y_1   ...   C :: k_n --> X_n        C :: v_n --> Y_n
----------------------------------------------------------------------------------------------
          C :: { k_1: v_1 ..., k_n: v_n } --> X_1, Y_1, X_2, Y_2...X_n, Y_n
```

#### Access Rules

All three object/array access rules refine the current constraint with an access.

##### Object index `.<s>` rule

```text
C :: .<s> --> C.s
```

##### Array index `.[<n>]` rule

```text
C :: .[<n>] --> C.[n]
```

##### Array iterator `.[]` rule

```text
C :: .[] --> C.[]
```

### Shape Inference

The construction of a constraint essentially gives us a unification problem. The constraint
language itself is very small.

```rust
pub enum Constraint {
    Diamond,                                // <>
    Sum(Box<Constraint>, Box<Constraint>),  // l + r
    In(Box<Constraint>, Box<Constraint>),   // l.r
    Field(String),                          // .<s>
    Array(Option<usize>),                   // .[] .[<i>]
}
```

The target shape is also similar.

```rust
pub enum Shape {
    Blob,                                   // <>
    Null,                                   // null
    Bool,                                   // bool
    Number,                                 // number
    String,                                 // string
    Array(Box<Shape>, Option<usize>),       // [T], [T ; n]
    Tuple(Vec<Shape>),                      // (T1, T2, ...)
    Object(Vec<(String, Shape)>),           // { s1: T1, s2: T2...}
}
```

The current constraint language actually targets a subset of the shape language, the correspondence
will increase further as we support more jq constructs.

The diamond constraint(`<>`) corresponds 1-1 to the blob shape(`<>`). A field access corresponds to an
object, an array iteration corresponds to an array, array accesses to shapes where unification is not possible
will construct tuples, but otherwise they set a minimum length of the array. In(`.`) corresponds to composition
of accesses, only leaving sum(`+`) without a direct corresponding structure.

The sum is the unification of two constraints. It merges the internals of the shapes, and runs a type error if
unification is not possible. Merging an array with an object is, for example, an invalid unification. Merging
two objects essentially means creating a new object with both their key-values, and merging the values for
any keys present in both objects.

The resulting of shape building gives us a `shape`, which we can now compare with a JSON. Right now, this
comparison is a bespoke implementation that follows the JSON along with the shape. Indeed, we can reuse the
unification enabled by the `sum` constraint for this comparison too. Shape comparison is just a bespoke example
of unification between two shapes, which can be interpreted back to constraints.

As we have a mechanism to compare the inferred shape with the JSON input, we can go back to the example I mentioned at the beginning:

```text
Input: [{"name": "John", "age": 25}, {"name": "Jane", "age": 30}]

Filter: .[] | .age, .name | {v: .a}
```

Where we previously got the local and confusing error

```bash
jq: error (at <unknown>): Cannot index number with string "a"
```

We can now get the global and more clear one.

```text
Shape mistmatch detected!
        at [0].age
        Expected: {a: <>}
        Got: 25
```

## Roadmap

The current implementation is a neat demo, but it's only semi-formalized and nowhere near the full jq. The following
steps require us to implement a larger subset of jq, while understanding the limitations of the current analysis
with further language constructs such as branches, functions, and recursion. We also need to work more on the formalization
and error propagation, because the current type errors produced with unification failures are very handwaved.

There is also the second part of the project≤, where we augment jq with optional type hints for the users to provide
type information.

### Language

- [ ] Parsing(we don't have a parser)
- [ ] Branches
- [ ] Function calls, recursion
- [ ] Error
- [ ] Empty
- [ ] Halt
- [ ] Arithmetic
- [ ] Recursive descent
- [ ] String Interpolation
- [ ] Variable Binding
- [ ] Try-catch/non-local control flow
- [ ] Assignment

### Analysis and Type System

- [ ] Branching
- [ ] Recursion, function inlining
- [ ] Type Error vs Error vs Empty vs Null vs Halt
- [ ] Variable binding
- [ ] Try-catch/non-local control flow
- [ ] Assignment

### Type Hints

- [ ] Syntax?
- [ ] How do they integrate to the type system?

