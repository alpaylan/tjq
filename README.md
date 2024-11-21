# Typed JQ

JQ is a successful tool for json manipulation.

```bash
$ jq -nc '[1, 2, 3] | map(select(. % 2 == 1))'
> [1, 3]
```

JQ has a problem though, it's errors are kind of hard to decipher. When you start creating some
internal data as part of some larger transformation, it might be hard to pinpoint the source
of an error.

```bash
$ jq -nc '[{"a": 1}, {"a": "b"}] | map(select(.a)) |   map(select(. % 2 == 1))'
> jq: error (at <unknown>): object ({"a":1}) and number (2) cannot be divided (remainder)
```

Here, the reason of the error is that `[{"a": 1}, {"a": "b"}] | map(select(.a))` evaluates to `[{"a":1},{"a":"b"}]`,
while I was trying to apply `.a`, which means I should've done `[{"a": 1}, {"a": "b"}] | map(.a)` in the first place.

Fixing that, we get the following error. Which is honestly fine to have.

```bash
$ jq -nc '[{"a": 1}, {"a": "b"}] | map(.a) |   map(select(. % 2 == 1))'      
> jq: error (at <unknown>): string ("b") and number (2) cannot be divided (remainder)
```

Though, if we think about the set of json inputs that this script would properly work on, we can actually get an abstract shape.

The corresponding typescript type would roughly be;

```typescript
type ValidInput = {
    "a": number
}[]
```

which means we expect an `array` of objects, where each object has the key `a`. After we analyze the jq filter and reach this conclusion,
we now don't have to resort to localized errors that might be hard to detect, as we can give a more structured static error message based
on the mismatch of the expected, and received input shapes.

Hence, the objective of the project is (1) to devise an analysis that'll take a given filter and return a shape, (2) compare a shape with a given json
and report mismatches.

For this purpose, we don't work with jq itself(which is not that complex, but still a large feat of engineering), but with a toy jq interpreter I've
started implementing.

```rust
enum Filter {
    Dot,                                      // .
    Pipe(Box<Filter>, Box<Filter>),           // <f_1> | <f_2>
    Comma(Box<Filter>, Box<Filter>),          // <f_1>, <f_2>
    Value(Json),                              // <j>
    ObjIndex(String),                         // .<s>
    ArrayIndex(usize),                        // .[<n>]
    ArrayIterator,                            // .[]
}
```

The interpreter supports a small core of jq, which roughly makes most of the parts I want to support for now, plus the easy stuff like arithmetic.

Given an input such as `[{name: John, age: 25}, {name: Jane, age: 30}]`, we can currently interpret the filter `.[] | .age, .name` to give the result

```text
25
John
30
Jane
```

So, how do we infer shapes for this filter? The idea relies on something akin to symbolic execution. As we execute a given filter, we build a set of constraints
on top of the input. At first, with no constraints, all json is valid. Each access constraints some part of the input json. Applying this idea on the provided example:

1. `.[]` assumes the input is an array, hence the input must be refined into an array.
2. `.[]` iterates over the array, so the next part of the filter `| <f_2>` will be the shape of the elements of the array.
3. `<f_1>, <f_2>` deduplicates the input, and applies both `f_1` and `f_2` on the input. So we must collect their constraints, and use both of them.
4. `.age` assumes the input has the field `age`, `.name` assumes the input has the field `name`, and `,` means we need both, so the end result of the inference is

```typescript
type ValidInput = {
    name: unknown,
    age: unknown,
}[]
```

## How to formalize this?

The formalization requires two things.

1. A way of expressing constraints
2. What constraints each construct produces

Let's start with a very simple constraint language. All arrays are homogeneous, all field accesses are required.

- `.a.[].b` implies `{"a": [{"b": _}]}`
- `.[].[],[]` imples `[[[_]]]`

The good thing about this is that constraints are easily composed

- `.a.b + .b.a` gives `{"a": {"b": _}, "b": {"a": _}}`

The second good thing about it is that we can just carry it within the execution and extend it.

Let's see how that works.

`.a | .b` starts with the empty constraint `<>`, for pipes, we execute the left side with the current constraint
`<> :: .a` producing `.a`, and execute the right side with the resulting constraint `.a :: .b` producing `.a.b`.

Going back to the original `.[] | .a, .b`

1. `<> :: .[] --> .[]` 
2. `C :: .a,.b --> (C :: .a) + (C :: .b)`
   1. `.[] :: .a --> .[].a`
   2. `.[] :: .a --> .[].b`
   3. `.[] :: .a,.b --> .[].a + .[].b`
3. `.[].a + .[].b ==> [{"a": _, "b": _}]`

Let's write the rules for each construct:

### Dot `.` rule

`C :: . --> C`

### Pipe `|` rule

```text

C :: f1 --> X       X :: f2 --> Y
---------------------------------
        C :: f1 | f2 --> Y

```

### Comma `,` rule

```text
C :: f1 --> X       C :: f2 --> Y
---------------------------------
        C :: f1, f2 --> X + Y
```

### Value `json` rule

An integer, boolean, string or null produces no constraints

```text
C :: i --> <>
C :: b --> <>
C :: s --> <>
C :: null --> <>
```

An array or object produces the sum of the constraints for their elements

#### Array `[]` rule

```text
    C :: v_1 --> X_1    C :: v_2 --> X_2   ...   C :: v_n --> X_n
---------------------------------------------------------------------
          C :: [ v_1 , v_2 ... v_n ] --> X_1, X_2...X_n
```

#### Object `{}` rule

```text
C :: k_1 --> X_1       C :: v_1 --> Y_1   ...   C :: k_n --> X_n        C :: v_n --> Y_n
----------------------------------------------------------------------------------------------
          C :: { k_1: v_1 ..., k_n: v_n } --> X_1, Y_1, X_2, Y_2...X_n, Y_n
```

### Object index `.<s>` rule

```text
C :: .<s> --> C.s
```

### Array index `.[<n>]` rule

```text
C :: .[<n>] --> C.[n]
```

### Array iterator `.[]` rule

```text
C :: .[] --> C.[]
```

### Shape Inference

Once we build up a constraint, we need to infer a shape from the constraint. In doing so,
we process the constraint in a tree-piercing style. At the beginning, we start with a `blob`, which is
essentially an unconstrained piece of JSON. As we process the constraints, we refine the `blob` into
a more concrete JSON-like object with `blob`s in unconstrained places.

Below is a demonstration of the processes:

```text
Input: [{"name": "John", "age": 25}, {"name": "Jane", "age": 30}]

Filter: .[] | .age, .name | {v: .a}

jq: error (at <unknown>): Cannot index number with string "a"

Constraint: (<> + ((<>.[].age + <>.[].name) + (<>.[].age + <>.[].name).a))

Shape: [{age: {a: <>}, name: {a: <>}}]

Shape mistmatch detected!
        at [0].age
        Expected: {a: <>}
        Got: 25
```

We give the input JSON `[{"name": "John", "age": 25}, {"name": "Jane", "age": 30}]` to
the filter `.[] | .age, .name | {v: .a}`, and the interpretation gives a local error,
`Cannot index number with string "a"`. We then process the filter to build up a constraint,
which is rather cryptic, but what it really is is a summation of the different access patterns
within the filter. We process the constraint to build up a `shape`, a semi-concrete JSON
with `blob`s in several places `[{age: {a: <>}, name: {a: <>}}]`. When we compare the input
with the shape, we get a global error this time, telling us the exact point the error occurs
and how.