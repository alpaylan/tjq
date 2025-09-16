use tower_lsp::lsp_types::{CompletionItem, CompletionItemKind, Position, InsertTextFormat};
use crate::document::DocumentData;
use crate::builtins::{get_builtins, get_keywords};
use tree_sitter::{Node, Point};
use std::collections::HashSet;

#[derive(Debug)]
enum TjqCompletionContext {
    AfterDot,
    AfterPipe,
    InExpression,
    InObject,
    Unknown,
}

pub struct CompletionProvider;

impl CompletionProvider {
    pub fn provide_completions(
        doc: &DocumentData,
        position: Position,
        _context: Option<tower_lsp::lsp_types::CompletionContext>,
    ) -> Vec<CompletionItem> {
        let mut completions = Vec::new();
        
        let context = Self::get_context_at_position(doc, position);
        
        // Add completions based on context
        match context {
            TjqCompletionContext::AfterDot => {
                completions.extend(Self::field_completions());
                completions.extend(Self::array_method_completions());
            }
            TjqCompletionContext::AfterPipe => {
                completions.extend(Self::builtin_completions());
                completions.extend(Self::filter_completions());
            }
            TjqCompletionContext::InExpression => {
                completions.extend(Self::builtin_completions());
                completions.extend(Self::keyword_completions());
                completions.extend(Self::variable_completions());
            }
            TjqCompletionContext::InObject => {
                completions.extend(Self::field_completions());
            }
            _ => {
                // Default: provide all completions
                completions.extend(Self::builtin_completions());
                completions.extend(Self::keyword_completions());
            }
        }
        
        completions
    }
    
    fn get_context_at_position(doc: &DocumentData, position: Position) -> TjqCompletionContext {
        // Get the text before cursor
        let line = position.line as usize;
        if line >= doc.rope.len_lines() {
            return TjqCompletionContext::Unknown;
        }
        
        let line_text = doc.rope.line(line).to_string();
        let char_pos = position.character as usize;
        let text_before = &line_text[..char_pos.min(line_text.len())];
        
        if text_before.ends_with('.') {
            TjqCompletionContext::AfterDot
        } else if text_before.ends_with('|') || text_before.ends_with("| ") {
            TjqCompletionContext::AfterPipe
        } else if text_before.contains('{') && !text_before.contains('}') {
            TjqCompletionContext::InObject
        } else {
            TjqCompletionContext::InExpression
        }
    }
    
    fn builtin_completions() -> Vec<CompletionItem> {
        let builtins = get_builtins();
        let mut items = Vec::new();
        
        for (name, info) in builtins.iter() {
            let mut item = CompletionItem::new_simple(
                name.to_string(),
                info.description.to_string(),
            );
            item.kind = Some(CompletionItemKind::FUNCTION);
            item.detail = Some(info.signature.to_string());
            
            // Add snippet for functions with parameters
            if info.signature.contains('(') {
                item.insert_text = Some(format!("{}($1)", name));
                item.insert_text_format = Some(InsertTextFormat::SNIPPET);
            }
            
            // Add documentation with examples
            let mut doc = format!("{}\n\n**Signature:** `{}`", info.description, info.signature);
            if !info.examples.is_empty() {
                doc.push_str("\n\n**Examples:**\n");
                for example in &info.examples {
                    doc.push_str(&format!("```jq\n{}\n```\n", example));
                }
            }
            item.documentation = Some(tower_lsp::lsp_types::Documentation::MarkupContent(
                tower_lsp::lsp_types::MarkupContent {
                    kind: tower_lsp::lsp_types::MarkupKind::Markdown,
                    value: doc,
                }
            ));
            
            items.push(item);
        }
        
        items
    }
    

fn keyword_completions() -> Vec<CompletionItem> {
    let keywords = get_keywords();
    let mut items = Vec::new();

    let keyword_snippets: Vec<(&str, &str)> = vec![
        ("if", "if ${1:condition} then\n\t${2:expression}\nelse\n\t${3:expression}\nend"),
        ("try", "try ${1:expression} catch ${2:null}"),
        ("def", "def ${1:name}(${2:params}):\n\t${3:body}\n;"),
        ("reduce", "reduce ${1:expression} as $${2:var} (${3:init}; ${4:update})"),
        ("foreach", "foreach ${1:expression} as $${2:var} (${3:init}; ${4:update}; ${5:extract})"),
        ("as", "${1:expression} as $${2:var} | ${3:expression}"),
        ("select", "select(${1:condition})"),
        ("map", "map(${1:expression})"),
        ("group_by", "group_by(${1:expression})"),
        ("sort_by", "sort_by(${1:expression})"),
        ("unique_by", "unique_by(${1:expression})"),
        ("min_by", "min_by(${1:expression})"),
        ("max_by", "max_by(${1:expression})"),
        ("while", "while(${1:condition}; ${2:update})"),
        ("until", "until(${1:condition}; ${2:update})"),
        ("limit", "limit(${1:n}; ${2:expression})"),
        ("first", "first(${1:expression})"),
        ("last", "last(${1:expression})"),
        ("range", "range(${1:from}; ${2:to})"),
        ("recurse", "recurse(${1:expression})"),
    ];

    let snippet_keys: HashSet<&str> = keyword_snippets.iter().map(|(k, _)| *k).collect();

    for &(keyword, snippet) in &keyword_snippets {
        let mut item = CompletionItem::new_simple(keyword.to_string(), format!("Keyword: {}", keyword));
        item.kind = Some(CompletionItemKind::KEYWORD);
        item.insert_text = Some(snippet.to_string());
        item.insert_text_format = Some(InsertTextFormat::SNIPPET);
        items.push(item);
    }

    for keyword in &keywords {
        if !snippet_keys.contains(keyword) {
            let mut item = CompletionItem::new_simple(keyword.to_string(), format!("Keyword: {}", keyword));
            item.kind = Some(CompletionItemKind::KEYWORD);
            items.push(item);
        }
    }

    items
}

    fn structural_completions() -> Vec<CompletionItem> {
        let mut items = Vec::new();
        
        // Common structural patterns with snippets
        let patterns = vec![
            // Conditionals
            ("if-then", "if ${1:condition} then ${2:expression} end", "Simple if statement"),
            ("if-then-else", "if ${1:condition} then\n\t${2:expression}\nelse\n\t${3:expression}\nend", "If-else statement"),
            ("if-elif-else", "if ${1:condition} then\n\t${2:expression}\nelif ${3:condition} then\n\t${4:expression}\nelse\n\t${5:expression}\nend", "If-elif-else statement"),
            
            // Error handling
            ("try-catch", "try ${1:expression} catch ${2:null}", "Try-catch error handling"),
            ("optional", "${1:expression}?", "Optional chaining"),
            ("alternative", "${1:expression} // ${2:default}", "Alternative operator"),
            
            // Loops and iteration
            ("foreach-loop", "foreach ${1:expression} as $${2:item} (${3:init}; ${4:update}; ${5:extract})", "Foreach loop"),
            ("reduce-sum", "reduce ${1:.} as $${2:x} (0; . + $${2:x})", "Sum using reduce"),
            ("reduce-collect", "reduce ${1:.} as $${2:x} ([]; . + [$${2:x}])", "Collect into array"),
            ("while-loop", "while(${1:condition}; ${2:update})", "While loop"),
            ("until-loop", "until(${1:condition}; ${2:update})", "Until loop"),
            
            // Array operations
            ("array-filter", ".[] | select(${1:condition})", "Filter array elements"),
            ("array-map", "map(${1:expression})", "Map over array"),
            ("array-slice", ".[${1:start}:${2:end}]", "Array slice"),
            ("array-index", ".[${1:0}]", "Array index access"),
            ("array-iterate", ".[]", "Iterate array elements"),
            
            // Object operations
            ("object-keys", "keys", "Get object keys"),
            ("object-values", ".[]", "Get object values"),
            ("object-entries", "to_entries", "Convert to key-value pairs"),
            ("object-from-entries", "from_entries", "Create object from entries"),
            ("object-merge", ". * ${1:other}", "Merge objects"),
            ("object-select", "{ ${1:key}: .${1:key} }", "Select object fields"),
            
            // Variable binding
            ("let-binding", "${1:expression} as $${2:var} | ${3:expression}", "Variable binding"),
            ("multi-binding", "(${1:expr1}, ${2:expr2}) as ($${3:var1}, $${4:var2}) | ${5:expression}", "Multiple variable binding"),
            
            // Function definition
            ("function", "def ${1:name}(${2:params}):\n\t${3:body}\n;", "Function definition"),
            ("function-recursive", "def ${1:name}(${2:params}):\n\t${3:base_case} // ${1:name}(${4:recursive_call})\n;", "Recursive function"),
            
            // Common filters
            ("not-null", "select(. != null)", "Filter non-null values"),
            ("is-number", "select(type == \"number\")", "Filter numbers"),
            ("is-string", "select(type == \"string\")", "Filter strings"),
            ("is-array", "select(type == \"array\")", "Filter arrays"),
            ("is-object", "select(type == \"object\")", "Filter objects"),
            
            // String operations
            ("string-split", "split(\"${1:delimiter}\")", "Split string"),
            ("string-join", "join(\"${1:delimiter}\")", "Join array to string"),
            ("string-contains", "contains(\"${1:substring}\")", "Check string contains"),
            ("string-regex", "test(\"${1:regex}\")", "Test regex match"),
            
            // Mathematical operations
            ("sum", "add", "Sum array elements"),
            ("average", "add / length", "Calculate average"),
            ("min-max", "[min, max]", "Get min and max"),
            
            // Grouping and aggregation
            ("group-by", "group_by(${1:expression})", "Group by expression"),
            ("unique", "unique", "Get unique elements"),
            ("unique-by", "unique_by(${1:expression})", "Get unique by expression"),
            ("sort", "sort", "Sort array"),
            ("sort-by", "sort_by(${1:expression})", "Sort by expression"),
            ("reverse", "reverse", "Reverse array"),
        ];
        
        for (label, snippet, description) in patterns {
            let mut item = CompletionItem::new_simple(
                label.to_string(),
                description.to_string(),
            );
            item.kind = Some(CompletionItemKind::SNIPPET);
            item.insert_text = Some(snippet.to_string());
            item.insert_text_format = Some(InsertTextFormat::SNIPPET);
            item.sort_text = Some(format!("00{}", label)); // Prioritize snippets
            items.push(item);
        }
        
        items
    }
    
    fn field_completions() -> Vec<CompletionItem> {
        // Common field access patterns
        vec![
            Self::create_completion("[]", "Array/object iterator", CompletionItemKind::OPERATOR),
            Self::create_completion("[0]", "First element", CompletionItemKind::OPERATOR),
            Self::create_completion("[-1]", "Last element", CompletionItemKind::OPERATOR),
            Self::create_completion("[:]", "Array slice", CompletionItemKind::OPERATOR),
            Self::create_completion("keys", "Object keys", CompletionItemKind::METHOD),
            Self::create_completion("values", "Object values", CompletionItemKind::METHOD),
            Self::create_completion("length", "Length", CompletionItemKind::METHOD),
        ]
    }
    
    fn array_method_completions() -> Vec<CompletionItem> {
        vec![
            Self::create_completion("[]", "Iterate array elements", CompletionItemKind::METHOD),
            Self::create_completion("length", "Array length", CompletionItemKind::METHOD),
            Self::create_completion("first", "First element", CompletionItemKind::METHOD),
            Self::create_completion("last", "Last element", CompletionItemKind::METHOD),
            Self::create_completion("reverse", "Reverse array", CompletionItemKind::METHOD),
            Self::create_completion("sort", "Sort array", CompletionItemKind::METHOD),
            Self::create_completion("unique", "Unique elements", CompletionItemKind::METHOD),
        ]
    }
    
    fn filter_completions() -> Vec<CompletionItem> {
        vec![
            Self::create_completion(".", "Identity", CompletionItemKind::OPERATOR),
            Self::create_completion(".[]", "Array/object iterator", CompletionItemKind::OPERATOR),
            Self::create_completion("select(.)", "Filter selection", CompletionItemKind::FUNCTION),
            Self::create_completion("map(.)", "Map transformation", CompletionItemKind::FUNCTION),
            Self::create_completion("not", "Logical negation", CompletionItemKind::FUNCTION),
        ]
    }
    
    fn variable_completions() -> Vec<CompletionItem> {
        vec![
            Self::create_completion("$__loc__", "Current location", CompletionItemKind::VARIABLE),
        ]
    }
    
    fn create_completion(label: &str, detail: &str, kind: CompletionItemKind) -> CompletionItem {
        let mut item = CompletionItem::new_simple(label.to_string(), detail.to_string());
        item.kind = Some(kind);
        item
    }
}
