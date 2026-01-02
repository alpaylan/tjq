use tower_lsp::lsp_types::{
    CodeAction, CodeActionKind, CodeActionContext, Range, TextEdit, WorkspaceEdit,
    Position, Url,
};
use std::collections::HashMap;
use crate::document::DocumentData;
// use tree_sitter::{Node, Point};

pub struct AssistProvider;

impl AssistProvider {
    pub fn provide_assists(
        doc: &DocumentData,
        range: Range,
        context: CodeActionContext,
        uri: Url,
    ) -> Vec<CodeAction> {
        let mut actions = Vec::new();
        
        // Extract to variable
        if let Some(action) = Self::extract_to_variable(doc, range, &uri) {
            actions.push(action);
        }
        
        // Wrap in try-catch
        if let Some(action) = Self::wrap_in_try_catch(doc, range, &uri) {
            actions.push(action);
        }
        
        // Convert to pipeline
        if let Some(action) = Self::convert_to_pipeline(doc, range, &uri) {
            actions.push(action);
        }
        
        // Add type check
        if let Some(action) = Self::add_type_check(doc, range, &uri) {
            actions.push(action);
        }
        
        // Simplify expression
        if let Some(action) = Self::simplify_expression(doc, range, &uri) {
            actions.push(action);
        }
        
        // Convert array access to safe access
        if let Some(action) = Self::safe_array_access(doc, range, &uri) {
            actions.push(action);
        }
        
        actions
    }
    
    fn extract_to_variable(doc: &DocumentData, range: Range, uri: &Url) -> Option<CodeAction> {
        let selected_text = Self::get_text_in_range(doc, range)?;
        
        if selected_text.len() < 3 || selected_text.contains('\n') {
            return None;
        }
        
        let new_text = format!("{} as $value | $value", selected_text);
        
        let mut changes = HashMap::new();
        changes.insert(
            uri.clone(),
            vec![TextEdit {
                range,
                new_text,
            }],
        );
        
        Some(CodeAction {
            title: "Extract to variable".to_string(),
            kind: Some(CodeActionKind::REFACTOR_EXTRACT),
            edit: Some(WorkspaceEdit {
                changes: Some(changes),
                ..Default::default()
            }),
            ..Default::default()
        })
    }
    
    fn wrap_in_try_catch(doc: &DocumentData, range: Range, uri: &Url) -> Option<CodeAction> {
        let selected_text = Self::get_text_in_range(doc, range)?;
        
        let new_text = format!("try {} catch null", selected_text);
        
        let mut changes = HashMap::new();
        changes.insert(
            uri.clone(),
            vec![TextEdit {
                range,
                new_text,
            }],
        );
        
        Some(CodeAction {
            title: "Wrap in try-catch".to_string(),
            kind: Some(CodeActionKind::REFACTOR),
            edit: Some(WorkspaceEdit {
                changes: Some(changes),
                ..Default::default()
            }),
            ..Default::default()
        })
    }
    
    fn convert_to_pipeline(doc: &DocumentData, range: Range, uri: &Url) -> Option<CodeAction> {
        let selected_text = Self::get_text_in_range(doc, range)?;
        
        // Check if it's a nested function call like map(select(...))
        if !selected_text.contains('(') || !selected_text.contains(')') {
            return None;
        }
        
        // Simple heuristic: convert map(select(condition)) to .[] | select(condition)
        if selected_text.starts_with("map(") && selected_text.ends_with(')') {
            let inner = &selected_text[4..selected_text.len()-1];
            let new_text = format!(".[] | {}", inner);
            
            let mut changes = HashMap::new();
            changes.insert(
                uri.clone(),
                vec![TextEdit {
                    range,
                    new_text,
                }],
            );
            
            return Some(CodeAction {
                title: "Convert to pipeline".to_string(),
                kind: Some(CodeActionKind::REFACTOR),
                edit: Some(WorkspaceEdit {
                    changes: Some(changes),
                    ..Default::default()
                }),
                ..Default::default()
            });
        }
        
        None
    }
    
    fn add_type_check(doc: &DocumentData, range: Range, uri: &Url) -> Option<CodeAction> {
        let selected_text = Self::get_text_in_range(doc, range)?;
        
        if selected_text.contains(' ') || selected_text.contains('\n') {
            return None;
        }
        
        let new_text = format!("{} | type", selected_text);
        
        let mut changes = HashMap::new();
        changes.insert(
            uri.clone(),
            vec![TextEdit {
                range,
                new_text,
            }],
        );
        
        Some(CodeAction {
            title: "Add type check".to_string(),
            kind: Some(CodeActionKind::REFACTOR),
            edit: Some(WorkspaceEdit {
                changes: Some(changes),
                ..Default::default()
            }),
            ..Default::default()
        })
    }
    
    fn simplify_expression(doc: &DocumentData, range: Range, uri: &Url) -> Option<CodeAction> {
        let selected_text = Self::get_text_in_range(doc, range)?;
        
        let simplified = match selected_text.as_str() {
            ".[] | ." => Some(".[]".to_string()),
            ". | ." => Some(".".to_string()),
            "select(. != null)" => Some("values".to_string()),
            "select(. == null)" => Some("nulls".to_string()),
            "if . then true else false end" => Some(". // false".to_string()),
            _ => None,
        }?;
        
        let mut changes = HashMap::new();
        changes.insert(
            uri.clone(),
            vec![TextEdit {
                range,
                new_text: simplified,
            }],
        );
        
        Some(CodeAction {
            title: "Simplify expression".to_string(),
            kind: Some(CodeActionKind::REFACTOR_REWRITE),
            edit: Some(WorkspaceEdit {
                changes: Some(changes),
                ..Default::default()
            }),
            ..Default::default()
        })
    }
    
    fn safe_array_access(doc: &DocumentData, range: Range, uri: &Url) -> Option<CodeAction> {
        let selected_text = Self::get_text_in_range(doc, range)?;
        
        // Check if it's array access like .[0] or .[-1]
        if !selected_text.starts_with(".[") || !selected_text.ends_with(']') {
            return None;
        }
        
        let new_text = format!("{}?", selected_text);
        
        let mut changes = HashMap::new();
        changes.insert(
            uri.clone(),
            vec![TextEdit {
                range,
                new_text,
            }],
        );
        
        Some(CodeAction {
            title: "Make array access safe".to_string(),
            kind: Some(CodeActionKind::REFACTOR),
            edit: Some(WorkspaceEdit {
                changes: Some(changes),
                ..Default::default()
            }),
            ..Default::default()
        })
    }
    
    fn get_text_in_range(doc: &DocumentData, range: Range) -> Option<String> {
        let start_line = range.start.line as usize;
        let end_line = range.end.line as usize;
        let start_char = range.start.character as usize;
        let end_char = range.end.character as usize;
        
        if start_line >= doc.rope.len_lines() || end_line >= doc.rope.len_lines() {
            return None;
        }
        
        if start_line == end_line {
            // Single line selection
            let line = doc.rope.line(start_line).to_string();
            if start_char <= line.len() && end_char <= line.len() {
                Some(line[start_char..end_char].to_string())
            } else {
                None
            }
        } else {
            // Multi-line selection
            let mut result = String::new();
            
            // First line
            let first_line = doc.rope.line(start_line).to_string();
            if start_char <= first_line.len() {
                result.push_str(&first_line[start_char..]);
            }
            
            // Middle lines
            for line_idx in (start_line + 1)..end_line {
                result.push('\n');
                result.push_str(&doc.rope.line(line_idx).to_string());
            }
            
            // Last line
            let last_line = doc.rope.line(end_line).to_string();
            if end_char <= last_line.len() {
                result.push('\n');
                result.push_str(&last_line[..end_char]);
            }
            
            Some(result)
        }
    }
}
