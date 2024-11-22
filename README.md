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

```json
[{"name": "John", "age": 25}, {"name": "Jane", "age": 30}]
```

The following jq program gets traverses the top level array, accesses both `name` and `age` fields,
and creates an object by accessing the `a` field of the produced values.

```jq
.[] | .age, .name | {v: .a}
```

This program is false though, `name` is a `string`, `age` is a `number`, they don't have a field `a`.
When we run the program, we get the following error from jq:

```bash
jq: error (at <unknown>): Cannot index number with string "a"
```

Using tjq, we can get a much better global error message:

```text
Shape mismatch detected!
        at [0].age
        Expected: {a: <>}
        Got: 25
```

As we process the jq program, we build up a `shape`, a semi-concrete JSON with holes.
`[{age: {a: <>}, name: {a: <>}}]`. After we build up the shape, we can now compare the
shape with the input to get global errors.

If the project is interesting to you, please checkout [docs.md](/docs.md), and leave a star!
