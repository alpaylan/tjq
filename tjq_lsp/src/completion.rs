// completion.rs - JQ completion assists using tree-sitter
use tree_sitter::{Node, Parser, Tree};
use std::collections::HashSet;
use tjq_parser::parse_defs;

/// Position in source code (line, column)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Position {
    pub line: u32,
    pub column: u32,
}

/// Text range for replacements
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TextRange {
    pub start: usize,
    pub end: usize,
}

/// Type of completion item
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompletionKind {
    Function,
    Builtin,
    Field,
    Keyword,
    Operator,
    Variable,
    Hole,
}

/// A completion suggestion
#[derive(Debug, Clone)]
pub struct CompletionItem {
    pub label: String,
    pub kind: CompletionKind,
    pub detail: Option<String>,
    pub documentation: Option<String>,
    pub insert_text: String,
    pub range: TextRange,
}

/// Context for completion - what are we completing?
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompletionContext {
    /// At the start of expression or after pipe
    ExpressionStart,
    /// After a dot (field access)
    FieldAccess { partial: String },
    /// Inside function call parentheses
    FunctionArgs,
    /// Object key position
    ObjectKey,
    /// After operators
    AfterOperator,
    /// Inside incomplete if/then/else
    IfExpression,
    /// After hole token
    AfterHole,
    /// Unknown/fallback context
    Unknown,
}

/// Main completion engine
pub struct CompletionEngine {
    /// Built-in jq functions (loaded from defs.jq)
    builtins: HashSet<String>,
    /// Common field names (could be extracted from data)
    common_fields: HashSet<&'static str>,
    /// jq keywords
    keywords: HashSet<&'static str>,
    /// Tree-sitter parser
    parser: Parser,
}

impl CompletionEngine {
    pub fn new() -> Self {
        let mut parser = Parser::new();
        parser
            .set_language(&tree_sitter_tjq::LANGUAGE.into())
            .expect("Error loading jq grammar");

        // Load builtins from defs.jq - handle the file path properly
        let defs_content = include_str!("../../tjq/defs.jq");
        let defs = parse_defs(defs_content);
        let builtins = defs.keys().cloned().collect();

        let mut common_fields = HashSet::new();
        // Common JSON field names
        common_fields.insert("id");
        common_fields.insert("name");
        common_fields.insert("title");
        common_fields.insert("description");
        common_fields.insert("value");
        common_fields.insert("data");
        common_fields.insert("items");
        common_fields.insert("results");
        common_fields.insert("status");
        common_fields.insert("type");
        common_fields.insert("created_at");
        common_fields.insert("updated_at");
        common_fields.insert("user");
        common_fields.insert("email");
        common_fields.insert("age");
        common_fields.insert("active");
        
        let mut keywords = HashSet::new();
        keywords.insert("if");
        keywords.insert("then");
        keywords.insert("else");
        keywords.insert("elif");
        keywords.insert("end");
        keywords.insert("and");
        keywords.insert("or");
        keywords.insert("not");
        keywords.insert("try");
        keywords.insert("catch");
        keywords.insert("as");
        keywords.insert("def");
        keywords.insert("reduce");
        keywords.insert("foreach");
        keywords.insert("while");
        keywords.insert("until");
        keywords.insert("null");
        keywords.insert("true");
        keywords.insert("false");
        
        Self {
            builtins,
            common_fields,
            keywords,
            parser,
        }
    }
    
    /// Get completions at a specific position in the source
    pub fn complete(&mut self, source: &str, position: Position) -> Vec<CompletionItem> {
        // Convert position to byte offset
        let offset = self.position_to_offset(source, position);
        
        let tree = match self.parser.parse(source, None) {
            Some(tree) => tree,
            None => {
                //if program fails return basic completions
                return self.get_basic_completions(offset);
            }
        };
        
        // Find the node/context at the cursor position and analyze it
        let root = tree.root_node();
        let context = self.find_and_analyze_node_at_offset(source, root, offset);
        
        // Generate completions based on context
        self.get_completions_for_context(&context, offset)
    }
    
    /// Convert line/column position to byte offset
    fn position_to_offset(&self, source: &str, position: Position) -> usize {
        let mut current_line = 0;
        let mut current_col = 0;
        
        for (offset, ch) in source.char_indices() {
            if current_line == position.line && current_col == position.column {
                return offset;
            }
            
            if ch == '\n' {
                current_line += 1;
                current_col = 0;
            } else {
                current_col += 1;
            }
        }
        
        source.len() // End of file
    }
    
    /// Find the most specific node that contains the offset and analyze it
    fn find_and_analyze_node_at_offset(&self, source: &str, root: Node, offset: usize) -> CompletionContext {
        let mut current = root;
        
        // Find the most specific node
        loop {
            let mut found_child = false;
            
            for i in 0..current.child_count() {
                if let Some(child) = current.child(i) {
                    if child.start_byte() <= offset && offset <= child.end_byte() {
                        current = child;
                        found_child = true;
                        break;
                    }
                }
            }
            
            if !found_child {
                break;
            }
        }
        
        self.analyze_node_context(source, current, offset)
    }
    
    /// Analyze context based on the node at cursor
    fn analyze_node_context(&self, source: &str, node: Node, offset: usize) -> CompletionContext {
        let node_kind = node.kind();
        let node_text = &source[node.start_byte()..node.end_byte()];
        
        match node_kind {
            "hole" => CompletionContext::AfterHole,
            "identifier" => {
                // Check if we're in the middle of typing an identifier
                if offset < node.end_byte() {
                    // Partial identifier 
                    CompletionContext::ExpressionStart
                } else {
                    CompletionContext::ExpressionStart
                }
            }
            "field" => {
                // Field access like .foo
                if node_text.starts_with('.') {
                    let partial = if offset > node.start_byte() + 1 {
                        source[node.start_byte() + 1..offset.min(node.end_byte())].to_string()
                    } else {
                        String::new()
                    };
                    CompletionContext::FieldAccess { partial }
                } else {
                    CompletionContext::ExpressionStart
                }
            }
            "dot" => {
                // Just typed a dot
                CompletionContext::FieldAccess { 
                    partial: String::new() 
                }
            }
            "pipeline" => {
                // Check if we're after the pipe symbol
                if let Some(pipe_child) = node.child(1) {
                    if offset >= pipe_child.end_byte() {
                        CompletionContext::ExpressionStart
                    } else {
                        CompletionContext::Unknown
                    }
                } else {
                    CompletionContext::Unknown
                }
            }
            "if_expression" => {
                self.analyze_if_expression_context(source, node, offset)
            }
            "call_expression" => {
                self.analyze_call_expression_context(source, node, offset)
            }
            "binary_expression" => {
                // Check if we're after an operator
                if let Some(op_node) = node.child(1) {
                    if offset >= op_node.end_byte() {
                        CompletionContext::AfterOperator
                    } else {
                        CompletionContext::ExpressionStart
                    }
                } else {
                    CompletionContext::ExpressionStart
                }
            }
            "ERROR" => {
                // Parse error - try to figure out context from parent or text
                if let Some(parent) = node.parent() {
                    self.analyze_node_context(source, parent, offset)
                } else {
                    self.analyze_text_context(source, offset)
                }
            }
            _ => {
                // Check parent context
                if let Some(parent) = node.parent() {
                    self.analyze_node_context(source, parent, offset)
                } else {
                    CompletionContext::ExpressionStart
                }
            }
        }
    }
    
    /// Analyze if expression context
    fn analyze_if_expression_context(&self, _source: &str, node: Node, offset: usize) -> CompletionContext {
        // Look for keywords like "then", "else", "end"
        for i in 0..node.child_count() {
            if let Some(child) = node.child(i) {
                if child.start_byte() <= offset && offset <= child.end_byte() {
                    match child.kind() {
                        "if" | "then" | "else" | "elif" | "end" => {
                            if offset == child.end_byte() {
                                return CompletionContext::IfExpression;
                            }
                        }
                        _ => {}
                    }
                }
            }
        }
        CompletionContext::IfExpression
    }
    
    /// Analyze call expression context (function arguments)
    fn analyze_call_expression_context(&self, _source: &str, node: Node, offset: usize) -> CompletionContext {
        // Check if we're inside the argument list
        if let Some(args_node) = node.child_by_field_name("arguments") {
            if args_node.start_byte() <= offset && offset <= args_node.end_byte() {
                return CompletionContext::FunctionArgs;
            }
        }
        CompletionContext::ExpressionStart
    }
    
    /// Fallback: analyze context based on text around cursor
    fn analyze_text_context(&self, source: &str, offset: usize) -> CompletionContext {
        let start = offset.saturating_sub(10);
        let end = (offset + 10).min(source.len());
        let context_text = &source[start..end];
        let cursor_in_context = offset - start;
        
        // Look for patterns before cursor
        let before_cursor = &context_text[..cursor_in_context];
        
        if before_cursor.ends_with('.') {
            CompletionContext::FieldAccess { partial: String::new() }
        } else if before_cursor.ends_with(" | ") || before_cursor.ends_with("|") {
            CompletionContext::ExpressionStart
        } else if before_cursor.ends_with("??") {
            CompletionContext::AfterHole
        } else if before_cursor.contains("if ") && !before_cursor.contains("end") {
            CompletionContext::IfExpression
        } else {
            CompletionContext::ExpressionStart
        }
    }
    
    /// Generate completions for a specific context
    fn get_completions_for_context(&self, context: &CompletionContext, offset: usize) -> Vec<CompletionItem> {
        match context {
            CompletionContext::ExpressionStart => {
                self.get_expression_completions(offset)
            }
            CompletionContext::FieldAccess { partial } => {
                self.get_field_completions(partial, offset)
            }
            CompletionContext::FunctionArgs => {
                self.get_argument_completions(offset)
            }
            CompletionContext::ObjectKey => {
                self.get_object_key_completions(offset)
            }
            CompletionContext::AfterOperator => {
                self.get_expression_completions(offset)
            }
            CompletionContext::IfExpression => {
                self.get_if_completions(offset)
            }
            CompletionContext::AfterHole => {
                self.get_hole_completions(offset)
            }
            CompletionContext::Unknown => {
                self.get_basic_completions(offset)
            }
        }
    }
    
    /// Get completions for start of expression
    fn get_expression_completions(&self, offset: usize) -> Vec<CompletionItem> {
        let mut completions = Vec::new();
        
        // Identity
        completions.push(CompletionItem {
            label: ".".to_string(),
            kind: CompletionKind::Operator,
            detail: Some("Identity".to_string()),
            documentation: Some("The identity filter - passes input unchanged".to_string()),
            insert_text: ".".to_string(),
            range: TextRange { start: offset, end: offset },
        });
        
        // Hole placeholder
        completions.push(CompletionItem {
            label: "??".to_string(),
            kind: CompletionKind::Hole,
            detail: Some("Hole".to_string()),
            documentation: Some("Placeholder for incomplete expression".to_string()),
            insert_text: "??".to_string(),
            range: TextRange { start: offset, end: offset },
        });
        
        // Built-in functions from defs.jq
        for builtin in &self.builtins {
            completions.push(CompletionItem {
                label: builtin.clone(),
                kind: CompletionKind::Builtin,
                detail: Some("Built-in function".to_string()),
                documentation: Some(format!("jq built-in function: {}", builtin)),
                insert_text: format!("{}(${{1}})", builtin), // LSP snippet with placeholder
                range: TextRange { start: offset, end: offset },
            });
        }
        
        // Keywords
        for &keyword in &self.keywords {
            completions.push(CompletionItem {
                label: keyword.to_string(),
                kind: CompletionKind::Keyword,
                detail: Some("Keyword".to_string()),
                documentation: Some(format!("jq keyword: {}", keyword)),
                insert_text: keyword.to_string(),
                range: TextRange { start: offset, end: offset },
            });
        }
        
        completions
    }
    
    /// Get field access completions
    fn get_field_completions(&self, partial: &str, offset: usize) -> Vec<CompletionItem> {
        let mut completions = Vec::new();
        
        // Filter common fields by partial match
        for &field in &self.common_fields {
            if field.starts_with(partial) {
                completions.push(CompletionItem {
                    label: field.to_string(),
                    kind: CompletionKind::Field,
                    detail: Some("Field access".to_string()),
                    documentation: Some(format!("Access field: {}", field)),
                    insert_text: field.to_string(),
                    range: TextRange { 
                        start: offset.saturating_sub(partial.len()),
                        end: offset 
                    },
                });
            }
        }
        
        // Array/object access patterns
        if "[]".starts_with(partial) {
            completions.push(CompletionItem {
                label: "[]".to_string(),
                kind: CompletionKind::Operator,
                detail: Some("Array iterator".to_string()),
                documentation: Some("Iterate over array elements".to_string()),
                insert_text: "[]".to_string(),
                range: TextRange { start: offset, end: offset },
            });
        }
        
        if "[0]".starts_with(partial) {
            completions.push(CompletionItem {
                label: "[0]".to_string(),
                kind: CompletionKind::Operator,
                detail: Some("Array index".to_string()),
                documentation: Some("Access first array element".to_string()),
                insert_text: "[${1:0}]".to_string(), // LSP snippet
                range: TextRange { start: offset, end: offset },
            });
        }
        
        completions
    }
    
    /// Get completions for if expressions
    fn get_if_completions(&self, offset: usize) -> Vec<CompletionItem> {
        let mut completions = Vec::new();
        
        // Keywords specific to if expressions
        for &keyword in &["then", "else", "elif", "end"] {
            completions.push(CompletionItem {
                label: keyword.to_string(),
                kind: CompletionKind::Keyword,
                detail: Some("If keyword".to_string()),
                documentation: Some(format!("if expression keyword: {}", keyword)),
                insert_text: keyword.to_string(),
                range: TextRange { start: offset, end: offset },
            });
        }
        
        // Also include regular expression completions
        completions.extend(self.get_expression_completions(offset));
        
        completions
    }
    
    /// Get completions after hole
    fn get_hole_completions(&self, offset: usize) -> Vec<CompletionItem> {
        // After a hole, suggest replacing it with actual expressions
        self.get_expression_completions(offset)
    }
    
    /// Get completions for function arguments
    fn get_argument_completions(&self, offset: usize) -> Vec<CompletionItem> {
        // For function arguments, return expression completions
        self.get_expression_completions(offset)
    }
    
    /// Get completions for object keys
    fn get_object_key_completions(&self, offset: usize) -> Vec<CompletionItem> {
        let mut completions = Vec::new();
        
        for &field in &self.common_fields {
            completions.push(CompletionItem {
                label: field.to_string(),
                kind: CompletionKind::Field,
                detail: Some("Object key".to_string()),
                documentation: Some(format!("Object key: {}", field)),
                insert_text: format!("{}: ${{{}}}", field, 1),
                range: TextRange { start: offset, end: offset },
            });
        }
        
        completions
    }
    
    /// Fallback basic completions
    fn get_basic_completions(&self, offset: usize) -> Vec<CompletionItem> {
        let mut completions = Vec::new();
        
        // Just offer identity and a few basics
        completions.push(CompletionItem {
            label: ".".to_string(),
            kind: CompletionKind::Operator,
            detail: Some("Identity".to_string()),
            documentation: Some("The identity filter".to_string()),
            insert_text: ".".to_string(),
            range: TextRange { start: offset, end: offset },
        });
        
        completions.push(CompletionItem {
            label: "??".to_string(),
            kind: CompletionKind::Hole,
            detail: Some("Hole".to_string()),
            documentation: Some("Placeholder for incomplete expression".to_string()),
            insert_text: "??".to_string(),
            range: TextRange { start: offset, end: offset },
        });
        
        completions.push(CompletionItem {
            label: "map".to_string(),
            kind: CompletionKind::Builtin,
            detail: Some("Built-in function".to_string()),
            documentation: Some("Map function over array elements".to_string()),
            insert_text: "map(${1})".to_string(),
            range: TextRange { start: offset, end: offset },
        });
        
        completions
    }
    
    /// Filter completions by prefix (for incremental completion)
    pub fn filter_completions(&self, completions: Vec<CompletionItem>, prefix: &str) -> Vec<CompletionItem> {
        if prefix.is_empty() {
            return completions;
        }
        
        completions
            .into_iter()
            .filter(|item| {
                item.label.to_lowercase().starts_with(&prefix.to_lowercase())
            })
            .collect()
    }
}

impl Default for CompletionEngine {
    fn default() -> Self {
        Self::new()
    }
}

/// Convenience function for getting completions
pub fn get_completions(source: &str, line: u32, column: u32) -> Vec<CompletionItem> {
    let mut engine = CompletionEngine::new();
    let position = Position { line, column };
    engine.complete(source, position)
}

#[cfg(test)]
mod tests {

    use super::*;
    
    #[test]
    fn test_completion_at_start() {
        let completions = get_completions("", 0, 0);
        assert!(!completions.is_empty());
        
        let has_identity = completions.iter().any(|c| c.label == ".");
        assert!(has_identity);
        
        let has_hole = completions.iter().any(|c| c.label == "??");
        assert!(has_hole);
    }
    
    #[test]
    fn test_field_completion() {
        let completions = get_completions(".", 0, 1);
        
        let has_fields = completions.iter().any(|c| c.kind == CompletionKind::Field);
        assert!(has_fields);
    }
    
    #[test]
    fn test_after_pipe() {
        let completions = get_completions(".foo | ", 0, 7);
        
        let has_functions = completions.iter().any(|c| c.kind == CompletionKind::Builtin);
        assert!(has_functions);
    }
    
    #[test]
    fn test_hole_completion() {
        //TODO Fix this
        let completions = get_completions("if ?? then", 0, 5);
        
        let has_expressions = completions.iter().any(|c| c.kind == CompletionKind::Builtin);
        assert!(has_expressions);
    }
    
    #[test]
    fn test_if_expression_completion() {
        let completions = get_completions("if true ", 0, 8);
        
        let has_then = completions.iter().any(|c| c.label == "then");
        assert!(has_then);
    }


    #[test]
    fn test_if_exression_completion2(){
        let completions = get_completions("if true then ", 0, 14);
        let has_end = completions.iter().any(|c| c.label == "end");
        assert!(has_end);
    }
}   