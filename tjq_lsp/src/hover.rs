use tower_lsp::lsp_types::{Hover, HoverContents, MarkupContent, MarkupKind, Position, Range};
use crate::document::DocumentData;
use crate::builtins::{get_builtins, get_keywords};
use tree_sitter::{Node, Point};

pub struct HoverProvider;

impl HoverProvider {
    pub fn provide_hover(doc: &DocumentData, position: Position) -> Option<Hover> {
        // Get the word at the current position
        let word = Self::get_word_at_position(doc, position)?;
        
        // Try to find hover information for the word
        if let Some(hover_content) = Self::get_hover_content(&word) {
            return Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: hover_content,
                }),
                range: None,
            });
        }
        
        // Try to get hover from syntax tree
        if let Some(tree) = &doc.tree {
            if let Some(node_hover) = Self::get_node_hover(doc, tree.root_node(), position) {
                return Some(node_hover);
            }
        }
        
        None
    }
    
    fn get_word_at_position(doc: &DocumentData, position: Position) -> Option<String> {
        let line = position.line as usize;
        if line >= doc.rope.len_lines() {
            return None;
        }
        
        let line_text = doc.rope.line(line).to_string();
        let char_pos = position.character as usize;
        
        if char_pos > line_text.len() {
            return None;
        }
        
        // Find word boundaries
        let mut start = char_pos;
        let mut end = char_pos;
        
        // Move start backwards to find word beginning
        while start > 0 {
            let ch = line_text.chars().nth(start - 1)?;
            if ch.is_alphanumeric() || ch == '_' || ch == '$' {
                start -= 1;
            } else {
                break;
            }
        }
        
        // Move end forward to find word end
        while end < line_text.len() {
            let ch = line_text.chars().nth(end)?;
            if ch.is_alphanumeric() || ch == '_' {
                end += 1;
            } else {
                break;
            }
        }
        
        if start < end {
            Some(line_text[start..end].to_string())
        } else {
            None
        }
    }
    
    fn get_hover_content(word: &str) -> Option<String> {
        // Check if it's a builtin function
        let builtins = get_builtins();
        if let Some(info) = builtins.get(word) {
            let mut content = format!("## {}\n\n", info.name);
            content.push_str(&format!("**Signature:** `{}`\n\n", info.signature));
            content.push_str(&format!("{}\n", info.description));
            
            if !info.examples.is_empty() {
                content.push_str("\n### Examples\n\n");
                for example in &info.examples {
                    content.push_str(&format!("```jq\n{}\n```\n", example));
                }
            }
            
            return Some(content);
        }
        
        // Check if it's a keyword
        let keywords = get_keywords();
        if keywords.contains(&word) {
            return Some(Self::get_keyword_documentation(word));
        }
        
        // Check for special operators
        match word {
            "." => Some("## Identity operator\n\nReturns the input unchanged.\n\n```jq\n[1,2,3] | .\n# Output: [1,2,3]\n```".to_string()),
            ".." => Some("## Recursive descent\n\nRecursively descends into all values.\n\n```jq\n{a: {b: 1}} | ..\n# Output: {\"a\":{\"b\":1}}, {\"b\":1}, 1\n```".to_string()),
            "[]" => Some("## Array/Object iterator\n\nIterates over array elements or object values.\n\n```jq\n[1,2,3] | .[]\n# Output: 1, 2, 3\n```".to_string()),
            "|" => Some("## Pipe operator\n\nPasses the output of one expression as input to the next.\n\n```jq\n[1,2,3] | length\n# Output: 3\n```".to_string()),
            "," => Some("## Comma operator\n\nProduces multiple outputs.\n\n```jq\n1, 2, 3\n# Output: 1, 2, 3\n```".to_string()),
            "//" => Some("## Alternative operator\n\nReturns the right side if the left side produces no output or errors.\n\n```jq\n.foo // \"default\"\n```".to_string()),
            _ => None,
        }
    }
    
    fn get_keyword_documentation(keyword: &str) -> String {
        match keyword {
            "if" => "## if-then-else\n\nConditional expression.\n\n**Syntax:**\n```jq\nif condition then\n  expression1\nelse\n  expression2\nend\n```".to_string(),
            
            "try" => "## try-catch\n\nError handling expression.\n\n**Syntax:**\n```jq\ntry expression catch handler\n```".to_string(),
            
            "def" => "## Function definition\n\nDefines a reusable function.\n\n**Syntax:**\n```jq\ndef name(params): body;\n```".to_string(),
            
            "as" => "## Variable binding\n\nBinds values to variables.\n\n**Syntax:**\n```jq\nexpr as $var | ...\n```".to_string(),
            
            "reduce" => "## Reduce expression\n\nFolds/reduces over inputs.\n\n**Syntax:**\n```jq\nreduce expr as $var (init; update)\n```".to_string(),
            
            "foreach" => "## Foreach expression\n\nIterates with state.\n\n**Syntax:**\n```jq\nforeach expr as $var (init; update; extract)\n```".to_string(),
            
            "and" => "## Logical AND\n\nReturns true if both operands are truthy.\n\n```jq\ntrue and false  # false\n```".to_string(),
            
            "or" => "## Logical OR\n\nReturns true if either operand is truthy.\n\n```jq\ntrue or false  # true\n```".to_string(),
            
            "not" => "## Logical NOT\n\nNegates a boolean value.\n\n```jq\nnot true  # false\n```".to_string(),
            
            "empty" => "## Empty\n\nProduces no output.\n\n```jq\n1, empty, 2  # Output: 1, 2\n```".to_string(),
            
            "null" => "## Null value\n\nThe null constant.\n\n```jq\nnull  # null\n```".to_string(),
            
            "true" => "## Boolean true\n\nThe boolean true constant.\n\n```jq\ntrue  # true\n```".to_string(),
            
            "false" => "## Boolean false\n\nThe boolean false constant.\n\n```jq\nfalse  # false\n```".to_string(),
            
            _ => format!("## {}\n\nTJQ keyword", keyword),
        }
    }
    
    fn get_node_hover(doc: &DocumentData, node: Node, position: Position) -> Option<Hover> {
        let point = Point::new(position.line as usize, position.character as usize);
        
        // Find the smallest node containing the position
        let target_node = Self::find_node_at_point(node, point)?;
        
        // Generate hover based on node type
        match target_node.kind() {
            "identifier" | "field_id" => {
                let text = Self::get_node_text(doc, target_node)?;
                Self::get_hover_content(&text).map(|content| Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: content,
                    }),
                    range: Some(Self::node_to_range(target_node)),
                })
            }
            "variable" => {
                let text = Self::get_node_text(doc, target_node)?;
                Some(Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: format!("## Variable: {}\n\nTJQ variable binding", text),
                    }),
                    range: Some(Self::node_to_range(target_node)),
                })
            }
            "string" => {
                Some(Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: "## String literal\n\nA TJQ string value".to_string(),
                    }),
                    range: Some(Self::node_to_range(target_node)),
                })
            }
            "number" => {
                let text = Self::get_node_text(doc, target_node)?;
                Some(Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: format!("## Number: {}\n\nNumeric literal", text),
                    }),
                    range: Some(Self::node_to_range(target_node)),
                })
            }
            "array" => {
                Some(Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: "## Array\n\nTJQ array literal".to_string(),
                    }),
                    range: Some(Self::node_to_range(target_node)),
                })
            }
            "object" => {
                Some(Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: "## Object\n\nTJQ object literal".to_string(),
                    }),
                    range: Some(Self::node_to_range(target_node)),
                })
            }
            _ => None,
        }
    }
    
    fn find_node_at_point(node: Node, point: Point) -> Option<Node> {
        if !Self::point_in_range(point, node.start_position(), node.end_position()) {
            return None;
        }
        
        // Try to find a more specific child node
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if let Some(found) = Self::find_node_at_point(child, point) {
                return Some(found);
            }
        }
        
        // Return this node if no child contains the point
        Some(node)
    }
    
    fn point_in_range(point: Point, start: Point, end: Point) -> bool {
        (point.row > start.row || (point.row == start.row && point.column >= start.column))
            && (point.row < end.row || (point.row == end.row && point.column <= end.column))
    }
    
    fn get_node_text(doc: &DocumentData, node: Node) -> Option<String> {
        let start = node.start_byte();
        let end = node.end_byte();
        doc.text.get(start..end).map(|s| s.to_string())
    }
    
    fn node_to_range(node: Node) -> Range {
        Range::new(
            Position::new(node.start_position().row as u32, node.start_position().column as u32),
            Position::new(node.end_position().row as u32, node.end_position().column as u32),
        )
    }
}
