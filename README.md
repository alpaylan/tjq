# Typed JQ

JQ is a successful tool for json manipulation.

```bash
$ jq -nc '[1, 2, 3] | map(select(. % 2 == 1))'
> [1, 3]
```

JQ has a problem though, it's errors are kind of hard to decipher. When you start creating some
internal data as part of some larger transformation, it might be hard to pinpoint the source
of an error. That is because JQ doesn't keep track of the input data flow, it merely interprets it.
So, when jq has an error, it is local.

For the input json below:

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
