use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct BuiltinInfo {
    pub name: &'static str,
    pub signature: &'static str,
    pub description: &'static str,
    pub examples: Vec<&'static str>,
}

pub fn get_builtins() -> HashMap<&'static str, BuiltinInfo> {
    let mut builtins = HashMap::new();
    
    // Core functions
    //TODO! -> extend
    builtins.insert("select", BuiltinInfo {
        name: "select",
        signature: "select(condition)",
        description: "Outputs the input if the condition is true, otherwise produces no output",
        examples: vec![
            "[1,2,3] | map(select(. > 1))",
            ".[] | select(.name == \"foo\")",
        ],
    });
    
    builtins.insert("empty", BuiltinInfo {
        name: "empty",
        signature: "empty",
        description: "Returns no outputs",
        examples: vec!["empty", "1, empty, 2"],
    });
    
    builtins.insert("map", BuiltinInfo {
        name: "map",
        signature: "map(f)",
        description: "Applies filter f to each element of the input array",
        examples: vec!["[1,2,3] | map(. * 2)", "[{a:1},{a:2}] | map(.a)"],
    });
    
    builtins.insert("add", BuiltinInfo {
        name: "add",
        signature: "add",
        description: "Adds/concatenates the elements of the input array",
        examples: vec!["[1,2,3] | add", "[\"a\",\"b\",\"c\"] | add"],
    });
    
    builtins.insert("length", BuiltinInfo {
        name: "length",
        signature: "length",
        description: "Returns the length of arrays, objects, strings, or null",
        examples: vec!["[1,2,3] | length", "\"{a:1}\" | length"],
    });
    
    builtins.insert("keys", BuiltinInfo {
        name: "keys",
        signature: "keys",
        description: "Returns the keys of an object as an array",
        examples: vec!["{a:1, b:2} | keys"],
    });
    
    builtins.insert("values", BuiltinInfo {
        name: "values",
        signature: "values",
        description: "Selects only non-null values",
        examples: vec!["[1, null, 2] | .[] | values"],
    });
    
    builtins.insert("type", BuiltinInfo {
        name: "type",
        signature: "type",
        description: "Returns the type of its input as a string",
        examples: vec!["1 | type", "\"hello\" | type", "null | type"],
    });
    
    builtins.insert("has", BuiltinInfo {
        name: "has",
        signature: "has(key)",
        description: "Tests whether the input object has the given key",
        examples: vec!["{a:1} | has(\"a\")", "{} | has(\"x\")"],
    });
    
    // Array functions
    builtins.insert("first", BuiltinInfo {
        name: "first",
        signature: "first",
        description: "Returns the first element",
        examples: vec!["[1,2,3] | first"],
    });
    
    builtins.insert("last", BuiltinInfo {
        name: "last",
        signature: "last",
        description: "Returns the last element",
        examples: vec!["[1,2,3] | last"],
    });
    
    builtins.insert("reverse", BuiltinInfo {
        name: "reverse",
        signature: "reverse",
        description: "Reverses an array",
        examples: vec!["[1,2,3] | reverse"],
    });
    
    builtins.insert("sort", BuiltinInfo {
        name: "sort",
        signature: "sort",
        description: "Sorts an array",
        examples: vec!["[3,1,2] | sort"],
    });
    
    builtins.insert("sort_by", BuiltinInfo {
        name: "sort_by",
        signature: "sort_by(f)",
        description: "Sorts an array by applying filter f to each element",
        examples: vec!["[{a:2},{a:1}] | sort_by(.a)"],
    });
    
    builtins.insert("group_by", BuiltinInfo {
        name: "group_by",
        signature: "group_by(f)",
        description: "Groups array elements by the result of filter f",
        examples: vec!["[1,2,3,4] | group_by(. % 2)"],
    });
    
    builtins.insert("unique", BuiltinInfo {
        name: "unique",
        signature: "unique",
        description: "Returns unique elements of an array",
        examples: vec!["[1,2,1,3,2] | unique"],
    });
    
    builtins.insert("unique_by", BuiltinInfo {
        name: "unique_by",
        signature: "unique_by(f)",
        description: "Returns unique elements by comparing results of filter f",
        examples: vec!["[{a:1,b:2},{a:1,b:3}] | unique_by(.a)"],
    });
    
    builtins.insert("flatten", BuiltinInfo {
        name: "flatten",
        signature: "flatten or flatten(depth)",
        description: "Flattens nested arrays",
        examples: vec!["[[1,2],[3,4]] | flatten", "[[[1]],[[2]]] | flatten(1)"],
    });
    
    builtins.insert("range", BuiltinInfo {
        name: "range",
        signature: "range(n) or range(from; to) or range(from; to; step)",
        description: "Generates a range of numbers",
        examples: vec!["range(3)", "range(1; 4)", "range(0; 10; 2)"],
    });
    
    // String functions
    builtins.insert("tostring", BuiltinInfo {
        name: "tostring",
        signature: "tostring",
        description: "Converts input to a string",
        examples: vec!["123 | tostring", "true | tostring"],
    });
    
    builtins.insert("tonumber", BuiltinInfo {
        name: "tonumber",
        signature: "tonumber",
        description: "Converts string to number",
        examples: vec!["\"123\" | tonumber", "\"3.14\" | tonumber"],
    });
    
    builtins.insert("split", BuiltinInfo {
        name: "split",
        signature: "split(separator)",
        description: "Splits a string by separator",
        examples: vec!["\"a,b,c\" | split(\",\")", "\"hello world\" | split(\" \")"],
    });
    
    builtins.insert("join", BuiltinInfo {
        name: "join",
        signature: "join(separator)",
        description: "Joins array elements with separator",
        examples: vec!["[\"a\",\"b\",\"c\"] | join(\",\")", "[1,2,3] | join(\"-\")"],
    });
    
    builtins.insert("startswith", BuiltinInfo {
        name: "startswith",
        signature: "startswith(str)",
        description: "Tests if string starts with given string",
        examples: vec!["\"hello\" | startswith(\"he\")", "\"world\" | startswith(\"wo\")"],
    });
    
    builtins.insert("endswith", BuiltinInfo {
        name: "endswith",
        signature: "endswith(str)",
        description: "Tests if string ends with given string",
        examples: vec!["\"hello\" | endswith(\"lo\")", "\"world\" | endswith(\"ld\")"],
    });
    
    builtins.insert("ltrimstr", BuiltinInfo {
        name: "ltrimstr",
        signature: "ltrimstr(str)",
        description: "Removes prefix string",
        examples: vec!["\"hello\" | ltrimstr(\"he\")"],
    });
    
    builtins.insert("rtrimstr", BuiltinInfo {
        name: "rtrimstr",
        signature: "rtrimstr(str)",
        description: "Removes suffix string",
        examples: vec!["\"hello\" | rtrimstr(\"lo\")"],
    });
    
    // Math functions
    builtins.insert("min", BuiltinInfo {
        name: "min",
        signature: "min",
        description: "Finds minimum value",
        examples: vec!["[3,1,2] | min"],
    });
    
    builtins.insert("max", BuiltinInfo {
        name: "max",
        signature: "max",
        description: "Finds maximum value",
        examples: vec!["[3,1,2] | max"],
    });
    
    builtins.insert("min_by", BuiltinInfo {
        name: "min_by",
        signature: "min_by(f)",
        description: "Finds element with minimum value of f",
        examples: vec!["[{a:3},{a:1}] | min_by(.a)"],
    });
    
    builtins.insert("max_by", BuiltinInfo {
        name: "max_by",
        signature: "max_by(f)",
        description: "Finds element with maximum value of f",
        examples: vec!["[{a:3},{a:1}] | max_by(.a)"],
    });
    
    builtins.insert("floor", BuiltinInfo {
        name: "floor",
        signature: "floor",
        description: "Rounds down to nearest integer",
        examples: vec!["3.7 | floor", "-2.3 | floor"],
    });
    
    builtins.insert("ceil", BuiltinInfo {
        name: "ceil",
        signature: "ceil",
        description: "Rounds up to nearest integer",
        examples: vec!["3.2 | ceil", "-2.7 | ceil"],
    });
    
    builtins.insert("round", BuiltinInfo {
        name: "round",
        signature: "round",
        description: "Rounds to nearest integer",
        examples: vec!["3.5 | round", "3.4 | round"],
    });
    
    builtins.insert("sqrt", BuiltinInfo {
        name: "sqrt",
        signature: "sqrt",
        description: "Square root",
        examples: vec!["9 | sqrt", "16 | sqrt"],
    });
    
    // Logical functions
    builtins.insert("not", BuiltinInfo {
        name: "not",
        signature: "not",
        description: "Logical negation",
        examples: vec!["true | not", "false | not"],
    });
    
    builtins.insert("all", BuiltinInfo {
        name: "all",
        signature: "all or all(condition)",
        description: "Tests if all elements are truthy or match condition",
        examples: vec!["[true, true] | all", "[1,2,3] | all(. > 0)"],
    });
    
    builtins.insert("any", BuiltinInfo {
        name: "any",
        signature: "any or any(condition)",
        description: "Tests if any element is truthy or matches condition",
        examples: vec!["[false, true] | any", "[1,2,3] | any(. > 2)"],
    });
    
    // Control flow
    // Note: "if", "try", "reduce", "foreach" are keywords and handled separately
    // We don't include them as builtins to avoid duplication
    
    // Type checking
    builtins.insert("isnumber", BuiltinInfo {
        name: "isnumber",
        signature: "isnumber",
        description: "Tests if input is a number",
        examples: vec!["1 | isnumber", "\"hello\" | isnumber"],
    });
    
    builtins.insert("isstring", BuiltinInfo {
        name: "isstring",
        signature: "isstring",
        description: "Tests if input is a string",
        examples: vec!["\"hello\" | isstring", "123 | isstring"],
    });
    
    builtins.insert("isarray", BuiltinInfo {
        name: "isarray",
        signature: "isarray",
        description: "Tests if input is an array",
        examples: vec!["[1,2] | isarray", "{} | isarray"],
    });
    
    builtins.insert("isobject", BuiltinInfo {
        name: "isobject",
        signature: "isobject",
        description: "Tests if input is an object",
        examples: vec!["{a:1} | isobject", "[] | isobject"],
    });
    
    builtins.insert("isboolean", BuiltinInfo {
        name: "isboolean",
        signature: "isboolean",
        description: "Tests if input is a boolean",
        examples: vec!["true | isboolean", "1 | isboolean"],
    });
    
    builtins.insert("isnull", BuiltinInfo {
        name: "isnull",
        signature: "isnull",
        description: "Tests if input is null",
        examples: vec!["null | isnull", "0 | isnull"],
    });
    
    builtins
}

pub fn get_keywords() -> Vec<&'static str> {
    vec![
        "if", "then", "else", "elif", "end",
        "and", "or", "not",
        "try", "catch",
        "as",
        "def",
        "reduce", "foreach",
        "module", "import", "include",
        "break", "label",
        "true", "false", "null",
        "empty",
    ]
}
