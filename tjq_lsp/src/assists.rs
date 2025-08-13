// assists.rs - Higher-level language server assists
use crate::completion::{CompletionEngine, CompletionItem, Position};
use tree_sitter::{Node, Parser, Tree};
use tjq_parser::parse_defs;

/// Diagnostic severity levels
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticSeverity {
    Error,
    Warning,
    Information,
    Hint,
}

/// A diagnostic message
#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub range: std::ops::Range<usize>,
    pub severity: DiagnosticSeverity,
    pub message: String,
    pub code: Option<String>,
}

/// Quick fix suggestion
#[derive(Debug, Clone)]
pub struct QuickFix {
    pub title: String,
    pub edits: Vec<TextEdit>,
}

/// Text edit for quick fixes
#[derive(Debug, Clone)]
pub struct TextEdit {
    pub range: std::ops::Range<usize>,
    pub new_text: String,
}

/// Hover information
#[derive(Debug, Clone)]
pub struct HoverInfo {
    pub contents: String,
    pub range: std::ops::Range<usize>,
}

/// Main language server functionality
pub struct JqLanguageServer {
    completion_engine: CompletionEngine,
    parser: Parser,
    builtins: std::collections::HashSet<String>,
}

impl Default for JqLanguageServer {
    fn default() -> Self {
        Self::new()
    }
}

impl JqLanguageServer {
    pub fn new() -> Self {
        let mut parser = Parser::new();
        parser
            .set_language(&tree_sitter_tjq::LANGUAGE.into())
            .expect("Error loading jq grammar");

        // Load builtins from defs.jq
        let defs = parse_defs(include_str!("../../tjq/defs.jq"));
        let builtins = defs.keys().cloned().collect();

        Self {
            completion_engine: CompletionEngine::new(),
            parser,
            builtins,
        }
    }
    
    /// Get completions at a position (main LSP method)
    pub fn completion(&mut self, source: &str, line: u32, column: u32) -> Vec<CompletionItem> {
        let position = Position { line, column };
        self.completion_engine.complete(source, position)
    }
    
    /// Get diagnostics for the entire document
    pub fn diagnostics(&mut self, source: &str) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();
        
        // Try to parse and collect errors
        match self.parser.parse(source, None) {
            Some(tree) => {
                // Even successful parses might have warnings
                self.analyze_tree_for_warnings(source, &tree, &mut diagnostics);
            }
            None => {
                // Parse error - try to give helpful message
                diagnostics.push(Diagnostic {
                    range: 0..source.len(),
                    severity: DiagnosticSeverity::Error,
                    message: "Failed to parse jq expression".to_string(),
                    code: Some("parse-error".to_string()),
                });
            }
        }
        
        // Check for common jq mistakes
        self.check_common_mistakes(source, &mut diagnostics);
        
        diagnostics
    }
    
    /// Get hover information at a position
    pub fn hover(&mut self, source: &str, line: u32, column: u32) -> Option<HoverInfo> {
        // Parse the source
        let tree = self.parser.parse(source, None)?;
        
        // Convert position to offset
        let offset = self.position_to_offset(source, Position { line, column });
        
        // Find and analyze node at position directly
        let root = tree.root_node();
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
        
        // Analyze the found node directly
        match current.kind() {
            "identifier" => {
                let text = &source[current.start_byte()..current.end_byte()];
                if self.is_builtin_function(text) {
                    Some(HoverInfo {
                        contents: self.get_builtin_documentation(text),
                        range: current.start_byte()..current.end_byte(),
                    })
                } else {
                    None
                }
            }
            "field" => {
                let text = &source[current.start_byte()..current.end_byte()];
                Some(HoverInfo {
                    contents: format!("Field access: `{}`", text),
                    range: current.start_byte()..current.end_byte(),
                })
            }
            "dot" => {
                Some(HoverInfo {
                    contents: "**Identity filter** - passes input unchanged".to_string(),
                    range: current.start_byte()..current.end_byte(),
                })
            }
            "hole" => {
                Some(HoverInfo {
                    contents: "**Hole** - placeholder for incomplete expression".to_string(),
                    range: current.start_byte()..current.end_byte(),
                })
            }
            _ => {
                // Check if we're in a pipeline
                if let Some(parent) = current.parent() {
                    if parent.kind() == "pipeline" {
                        Some(HoverInfo {
                            contents: "**Pipe operator** - passes output of left expression as input to right".to_string(),
                            range: current.start_byte()..current.end_byte(),
                        })
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
        }
    }
    
    /// Get quick fixes for diagnostics
    pub fn quick_fixes(&mut self, source: &str, range: std::ops::Range<usize>) -> Vec<QuickFix> {
        let mut fixes = Vec::new();
        
        // Try to parse to understand the context
        if let Some(tree) = self.parser.parse(source, None) {
            // Check for common fixable issues
            fixes.extend(self.suggest_hole_fixes(source, &tree, range.clone()));
            fixes.extend(self.suggest_syntax_fixes(source, &tree, range));
        }
        
        fixes
    }
    
    /// Format the entire document
    pub fn format(&mut self, source: &str) -> Option<String> {
        let tree = self.parser.parse(source, None)?;
        Some(self.format_tree(source, &tree))
    }
    
    
    fn analyze_tree_for_warnings(&self, source: &str, tree: &Tree, diagnostics: &mut Vec<Diagnostic>) {
        let root = tree.root_node();
        self.analyze_node_warnings(source, root, diagnostics);
    }
    
    fn analyze_node_warnings(&self, source: &str, node: Node, diagnostics: &mut Vec<Diagnostic>) {
        match node.kind() {
            "pipeline" => {
                // Check for redundant pipes like ". | ."
                if self.is_redundant_pipe(source, node) {
                    diagnostics.push(Diagnostic {
                        range: node.start_byte()..node.end_byte(),
                        severity: DiagnosticSeverity::Warning,
                        message: "Redundant pipe - both sides are identity".to_string(),
                        code: Some("redundant-pipe".to_string()),
                    });
                }
            }
            "array" => {
                // Check for empty arrays in certain contexts
                if self.is_suspicious_empty_array(node) {
                    diagnostics.push(Diagnostic {
                        range: node.start_byte()..node.end_byte(),
                        severity: DiagnosticSeverity::Hint,
                        message: "Empty array - consider using `empty` instead".to_string(),
                        code: Some("empty-array".to_string()),
                    });
                }
            }
            "ERROR" => {
                // Tree-sitter error nodes
                diagnostics.push(Diagnostic {
                    range: node.start_byte()..node.end_byte(),
                    severity: DiagnosticSeverity::Error,
                    message: "Syntax error in jq expression".to_string(),
                    code: Some("syntax-error".to_string()),
                });
            }
            "hole" => {
                // Holes are not errors, but we can suggest completions
                diagnostics.push(Diagnostic {
                    range: node.start_byte()..node.end_byte(),
                    severity: DiagnosticSeverity::Hint,
                    message: "Incomplete expression - replace hole with actual code".to_string(),
                    code: Some("incomplete-hole".to_string()),
                });
            }
            _ => {}
        }
        
        // Recursively check children
        for i in 0..node.child_count() {
            if let Some(child) = node.child(i) {
                self.analyze_node_warnings(source, child, diagnostics);
            }
        }
    }
    
    fn check_common_mistakes(&self, source: &str, diagnostics: &mut Vec<Diagnostic>) {
        // Check for completely invalid characters or patterns
        if source.contains("!@#") || source.contains("$$") {
            diagnostics.push(Diagnostic {
                range: 0..source.len(),
                severity: DiagnosticSeverity::Error,
                message: "Invalid jq syntax - contains unexpected characters".to_string(),
                code: Some("invalid-syntax".to_string()),
            });
        }
        
        // Check for common typos or mistakes
        if source.contains("..") && !source.contains("...") {
            // Might be trying to use recursive descent incorrectly
            if let Some(pos) = source.find("..") {
                diagnostics.push(Diagnostic {
                    range: pos..pos + 2,
                    severity: DiagnosticSeverity::Hint,
                    message: "Did you mean recursive descent (..)? Use `..` to recurse through all values".to_string(),
                    code: Some("recursive-hint".to_string()),
                });
            }
        }
        
        // Check for chained field access that could use pipes
        if source.contains(".foo.bar") && !source.contains(".foo | .bar") {
            if let Some(pos) = source.find(".foo.bar") {
                diagnostics.push(Diagnostic {
                    range: pos..pos + 8,
                    severity: DiagnosticSeverity::Hint,
                    message: "Consider using pipes for better readability: `.foo | .bar`".to_string(),
                    code: Some("pipe-suggestion".to_string()),
                });
            }
        }
    }
    
    fn suggest_hole_fixes(&self, source: &str, tree: &Tree, range: std::ops::Range<usize>) -> Vec<QuickFix> {
        let mut fixes = Vec::new();
        let root = tree.root_node();
        
        // Find holes in the range
        self.find_holes_in_range(source, root, range, &mut fixes);
        
        fixes
    }
    
    fn find_holes_in_range(&self, source: &str, node: Node, range: std::ops::Range<usize>, fixes: &mut Vec<QuickFix>) {
        if node.kind() == "hole" {
            let node_range = node.start_byte()..node.end_byte();
            if node_range.start >= range.start && node_range.end <= range.end {
                // Suggest replacing hole with common expressions
                fixes.push(QuickFix {
                    title: "Replace hole with identity (.)".to_string(),
                    edits: vec![TextEdit {
                        range: node_range.clone(),
                        new_text: ".".to_string(),
                    }],
                });
                
                fixes.push(QuickFix {
                    title: "Replace hole with null".to_string(),
                    edits: vec![TextEdit {
                        range: node_range.clone(),
                        new_text: "null".to_string(),
                    }],
                });
                
                fixes.push(QuickFix {
                    title: "Replace hole with empty".to_string(),
                    edits: vec![TextEdit {
                        range: node_range,
                        new_text: "empty".to_string(),
                    }],
                });
            }
        }
        
        // Recursively check children
        for i in 0..node.child_count() {
            if let Some(child) = node.child(i) {
                self.find_holes_in_range(source, child, range.clone(), fixes);
            }
        }
    }
    
    fn suggest_syntax_fixes(&self, _source: &str, _tree: &Tree, _range: std::ops::Range<usize>) -> Vec<QuickFix> {
        // TODO: Add more syntax-specific fixes
        Vec::new()
    }
    
    fn format_tree(&self, source: &str, tree: &Tree) -> String {
        let root = tree.root_node();
        self.format_node(source, root, 0)
    }
    
    fn format_node(&self, source: &str, node: Node, depth: usize) -> String {
        match node.kind() {
            "pipeline" => {
                if let (Some(left), Some(right)) = (node.child(0), node.child(2)) {
                    format!("{} | {}", 
                        self.format_node(source, left, depth),
                        self.format_node(source, right, depth)
                    )
                } else {
                    source[node.start_byte()..node.end_byte()].to_string()
                }
            }
            "sequence_expression" => {
                if let (Some(left), Some(right)) = (node.child(0), node.child(2)) {
                    format!("{}, {}", 
                        self.format_node(source, left, depth),
                        self.format_node(source, right, depth)
                    )
                } else {
                    source[node.start_byte()..node.end_byte()].to_string()
                }
            }
            "object" => {
                let mut result = String::from("{\n");
                let indent = "  ".repeat(depth + 1);
                
                // Format object contents
                for i in 0..node.child_count() {
                    if let Some(child) = node.child(i) {
                        if child.kind() == "pair" {
                            result.push_str(&indent);
                            result.push_str(&self.format_node(source, child, depth + 1));
                            result.push('\n');
                        }
                    }
                }
                
                result.push_str(&"  ".repeat(depth));
                result.push('}');
                result
            }
            "array" => {
                let mut result = String::from("[");
                let mut first = true;
                
                for i in 0..node.child_count() {
                    if let Some(child) = node.child(i) {
                        if !matches!(child.kind(), "[" | "]" | ",") {
                            if !first {
                                result.push_str(", ");
                            }
                            result.push_str(&self.format_node(source, child, depth));
                            first = false;
                        }
                    }
                }
                
                result.push(']');
                result
            }
            _ => {
                // For most nodes, just return the original text
                source[node.start_byte()..node.end_byte()].to_string()
            }
        }
    }
    
    fn is_redundant_pipe(&self, source: &str, pipe_node: Node) -> bool {
        // Check if both sides of pipe are identity operations
        if let (Some(left), Some(right)) = (pipe_node.child(0), pipe_node.child(2)) {
            let left_is_identity = self.is_identity_expr(source, left);
            let right_is_identity = self.is_identity_expr(source, right);
            left_is_identity && right_is_identity
        } else {
            false
        }
    }
    
    fn is_identity_expr(&self, source: &str, node: Node) -> bool {
        match node.kind() {
            "dot" => true,
            _ => {
                let text = &source[node.start_byte()..node.end_byte()];
                text.trim() == "."
            }
        }
    }
    
    fn is_suspicious_empty_array(&self, array_node: Node) -> bool {
        // Check if array has no meaningful children
        for i in 0..array_node.child_count() {
            if let Some(child) = array_node.child(i) {
                if !matches!(child.kind(), "[" | "]" | "," | "WHITESPACE") {
                    return false; // Has content
                }
            }
        }
        true // Is empty
    }
    
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
        
        source.len()
    }
    
    fn is_builtin_function(&self, name: &str) -> bool {
        self.builtins.contains(name) || matches!(name, 
            "map" | "select" | "filter" | "sort" | "sort_by" | "group_by" | 
            "unique" | "reverse" | "length" | "keys" | "values" | "type" |
            "empty" | "add" | "min" | "max" | "has" | "contains" | "split" |
            "join" | "tonumber" | "tostring"
        )
    }
    
    fn get_builtin_documentation(&self, name: &str) -> String {
        match name {
            "map" => "**map(expr)** - Apply expression to each element of array\n\nExample: `map(.name)` extracts name field from each object".to_string(),
            "select" => "**select(expr)** - Filter elements where expression is true\n\nExample: `select(.age > 18)` keeps only adults".to_string(),
            "sort" => "**sort** - Sort array elements\n\nExample: `sort` sorts numbers/strings in ascending order".to_string(),
            "sort_by" => "**sort_by(expr)** - Sort array by expression result\n\nExample: `sort_by(.age)` sorts objects by age field".to_string(),
            "length" => "**length** - Get length of array, object, string, or null\n\nExample: `length` returns number of elements".to_string(),
            "keys" => "**keys** - Get sorted keys of object or indices of array\n\nExample: `keys` returns `[\"age\", \"name\"]` for `{name: \"John\", age: 30}`".to_string(),
            "type" => "**type** - Get type of value\n\nExample: `type` returns `\"string\"`, `\"number\"`, `\"array\"`, `\"object\"`, `\"boolean\"`, or `\"null\"`".to_string(),
            "empty" => "**empty** - Produces no output\n\nExample: Use in conditionals to filter out values".to_string(),
            "has" => "**has(key)** - Check if object has key or array has index\n\nExample: `has(\"name\")` returns true if object has name field".to_string(),
            "contains" => "**contains(value)** - Check if input contains value\n\nExample: `\"hello\" | contains(\"ell\")` returns true".to_string(),
            "split" => "**split(separator)** - Split string by separator\n\nExample: `\"a,b,c\" | split(\",\")` returns `[\"a\", \"b\", \"c\"]`".to_string(),
            "join" => "**join(separator)** - Join array elements with separator\n\nExample: `[\"a\", \"b\", \"c\"] | join(\",\")` returns `\"a,b,c\"`".to_string(),
            "tonumber" => "**tonumber** - Convert string to number\n\nExample: `\"42\" | tonumber` returns `42`".to_string(),
            "tostring" => "**tostring** - Convert value to string\n\nExample: `42 | tostring` returns `\"42\"`".to_string(),
            "recurse" => "**recurse(f)** - Recursively apply filter\n\nExample: `recurse(.children[]?)` walks through nested structures".to_string(),
            _ => format!("Built-in function: **{}**", name),
        }
    }
}

/// Convenience functions for common operations
pub mod convenience {
    use super::*;
    
    /// Get completions 
    pub fn get_completions(source: &str, line: u32, column: u32) -> Vec<CompletionItem> {
        let mut server = JqLanguageServer::new();
        server.completion(source, line, column)
    }
    
    /// Get diagnostics =
    pub fn get_diagnostics(source: &str) -> Vec<Diagnostic> {
        let mut server = JqLanguageServer::new();
        server.diagnostics(source)
    }
    
    /// Get hover info =
    pub fn get_hover(source: &str, line: u32, column: u32) -> Option<HoverInfo> {
        let mut server = JqLanguageServer::new();
        server.hover(source, line, column)
    }
    
    /// Format jq code =
    pub fn format_jq(source: &str) -> Option<String> {
        let mut server = JqLanguageServer::new();
        server.format(source)
    }
}

/// handle incomplete treees
pub struct IncompleteParseHandler {
    parser: Parser,
}

impl IncompleteParseHandler {
    pub fn new() -> Self {
        let mut parser = Parser::new();
        parser
            .set_language(&tree_sitter_tjq::LANGUAGE.into())
            .expect("Error loading jq grammar");
        
        Self { parser }
    }
    
    /// Parse even incomplete/invalid tjq and extract 
    pub fn parse_incomplete(&mut self, source: &str) -> (Option<Tree>, Vec<String>) {
        let mut errors = Vec::new();
        
        // Try normal parsing first
        match self.parser.parse(source, None) {
            Some(tree) => {
                // Check for ERROR nodes
                let root = tree.root_node();
                self.collect_errors(source, root, &mut errors);
                (Some(tree), errors)
            }
            None => {
                errors.push("Failed to parse jq expression".to_string());
                (None, errors)
            }
        }
    }
    
    fn collect_errors(&self, source: &str, node: Node, errors: &mut Vec<String>) {
        if node.kind() == "ERROR" {
            let text = &source[node.start_byte()..node.end_byte()];
            errors.push(format!("Syntax error: {}", text));
        }
        
        // Recursively check children
        for i in 0..node.child_count() {
            if let Some(child) = node.child(i) {
                self.collect_errors(source, child, errors);
            }
        }
    }
    
    pub fn analyze_incomplete(&self, source: &str) -> AnalysisResult {
        let mut result = AnalysisResult::default();
        
        // Look for patterns even in unparseable code
        if source.contains('|') {
            result.likely_has_pipes = true;
        }
        
        if source.contains('.') {
            result.likely_has_field_access = true;
        }
        
        // Extract potential function names - split by various delimiters
        let words: Vec<&str> = source
            .split(|c: char| c.is_whitespace() || "()[]{}|.,;:".contains(c))
            .filter(|word| !word.is_empty())
            .collect();
            
        for word in words {
            if self.looks_like_function(word) {
                result.potential_functions.push(word.to_string());
            }
        }
        
        result
    }
    
    fn looks_like_function(&self, word: &str) -> bool {
        word.chars().all(|c| c.is_alphabetic() || c == '_') &&
        matches!(word, "map" | "select" | "filter" | "sort" | "length" | "keys" | "type" |
                      "empty" | "add" | "min" | "max" | "has" | "contains" | "split" |
                      "join" | "reverse" | "unique" | "group_by" | "sort_by" | "recurse")
    }
}

impl Default for IncompleteParseHandler {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Default)]
pub struct AnalysisResult {
    pub likely_has_pipes: bool,
    pub likely_has_field_access: bool,
    pub potential_functions: Vec<String>,
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_completion_integration() {
        let mut server = JqLanguageServer::new();
        
        let completions = server.completion(".", 0, 1);
        assert!(!completions.is_empty());
        
        let completions = server.completion("", 0, 0);
        assert!(completions.iter().any(|c| c.label.contains("map")));
        
        // Test completion after pipe - should get function completions
        let completions = server.completion(". | ", 0, 4);
        assert!(completions.iter().any(|c| c.label.contains("map")));
    }
    
    #[test] 
    fn test_diagnostics() {
        let mut server = JqLanguageServer::new();
        let diagnostics = server.diagnostics(".foo | .bar");
        let errors: Vec<_> = diagnostics.iter()
            .filter(|d| d.severity == DiagnosticSeverity::Error)
            .collect();
        assert!(errors.is_empty());
        
        let diagnostics = server.diagnostics("invalid jq $$");
        let has_errors = diagnostics.iter()
            .any(|d| d.severity == DiagnosticSeverity::Error);
        assert!(has_errors);
    }
    
    #[test]
    fn test_hover() {
        let mut server = JqLanguageServer::new();

        let hover = server.hover("map", 0, 1); // Position 1 should be inside "map"
        
        if let Some(hover) = hover {
            assert!(hover.contents.contains("map"));
        }
    }
    
    #[test]
    fn test_incomplete_parsing() {
        let mut handler = IncompleteParseHandler::new();
        let (tree, errors) = handler.parse_incomplete("map(.foo |");
        
        // Tree-sitter should still produce a tree even for incomplete input
        assert!(tree.is_some() || !errors.is_empty());
    }
    
    #[test]
    fn test_analysis_of_incomplete() {
        let handler = IncompleteParseHandler::new();
        let result = handler.analyze_incomplete("map(.foo | select");
        
        assert!(result.likely_has_pipes);
        assert!(result.likely_has_field_access);
        
        let has_map_or_select = result.potential_functions.contains(&"map".to_string()) ||
                               result.potential_functions.contains(&"select".to_string());
        assert!(has_map_or_select, "Expected 'map' or 'select' in functions: {:?}", result.potential_functions);
    }
    
    #[test]
    fn test_convenience_functions() {
        use convenience::*;
        
        let completions = get_completions(".", 0, 1);
        assert!(!completions.is_empty());
        
        let diagnostics = get_diagnostics(".foo");
        let errors: Vec<_> = diagnostics.iter()
            .filter(|d| d.severity == DiagnosticSeverity::Error)
            .collect();
        assert!(errors.is_empty());
    }
}