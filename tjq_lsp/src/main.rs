// crates/jq-lsp/src/main.rs
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{self, *};
use tower_lsp::{Client, LanguageServer, LspService, Server};
use std::collections::HashMap;
use tokio::sync::RwLock;

mod completion;
mod assists;

use assists::{JqLanguageServer, convenience};

pub struct JqLsp {
    client: Client,
    server: RwLock<JqLanguageServer>, // Wrap in RwLock for async access
    document_map: RwLock<HashMap<Url, String>>,
}

impl JqLsp {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            server: RwLock::new(JqLanguageServer::new()),
            document_map: RwLock::new(HashMap::new()),
        }
    }
    
    // /// Convert LSP position to internal position type
    // fn lsp_position_to_position(pos: &lsp_types::Position) -> completion::Position {
    //     completion::Position {
    //         line: pos.line,
    //         column: pos.character,
    //     }
    // }
    
    /// Convert our TextRange to LSP Range
    fn text_range_to_lsp_range(text: &str, range: std::ops::Range<usize>) -> Range {
        let start_pos = offset_to_position(text, range.start);
        let end_pos = offset_to_position(text, range.end);
        
        Range {
            start: Position {
                line: start_pos.0,
                character: start_pos.1,
            },
            end: Position {
                line: end_pos.0,
                character: end_pos.1,
            },
        }
    }
    
    /// Convert our CompletionItem to LSP CompletionItem
    fn completion_item_to_lsp(item: &completion::CompletionItem, text: &str) -> CompletionItem {
        let kind = match item.kind {
            completion::CompletionKind::Function => CompletionItemKind::FUNCTION,
            completion::CompletionKind::Builtin => CompletionItemKind::FUNCTION,
            completion::CompletionKind::Field => CompletionItemKind::FIELD,
            completion::CompletionKind::Keyword => CompletionItemKind::KEYWORD,
            completion::CompletionKind::Operator => CompletionItemKind::OPERATOR,
            completion::CompletionKind::Variable => CompletionItemKind::VARIABLE,
            completion::CompletionKind::Hole => CompletionItemKind::SNIPPET,
        };
        
        let range = Self::text_range_to_lsp_range(text, item.range.start..item.range.end);
        
        CompletionItem {
            label: item.label.clone(),
            kind: Some(kind),
            detail: item.detail.clone(),
            documentation: item.documentation.as_ref().map(|doc| {
                Documentation::MarkupContent(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: doc.clone(),
                })
            }),
            insert_text: Some(item.insert_text.clone()),
            text_edit: Some(CompletionTextEdit::Edit(TextEdit {
                range,
                new_text: item.insert_text.clone(),
            })),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..Default::default()
        }
    }
    
    /// Convert our Diagnostic to LSP Diagnostic
    fn diagnostic_to_lsp(diagnostic: &assists::Diagnostic, text: &str) -> Diagnostic {
        let severity = match diagnostic.severity {
            assists::DiagnosticSeverity::Error => DiagnosticSeverity::ERROR,
            assists::DiagnosticSeverity::Warning => DiagnosticSeverity::WARNING,
            assists::DiagnosticSeverity::Information => DiagnosticSeverity::INFORMATION,
            assists::DiagnosticSeverity::Hint => DiagnosticSeverity::HINT,
        };
        
        let range = Self::text_range_to_lsp_range(text, diagnostic.range.clone());
        
        Diagnostic {
            range,
            severity: Some(severity),
            code: diagnostic.code.as_ref().map(|c| NumberOrString::String(c.clone())),
            source: Some("jq-lsp".to_string()),
            message: diagnostic.message.clone(),
            ..Default::default()
        }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for JqLsp {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: Some(ServerInfo {
                name: "jq-lsp".to_string(),
                version: Some("0.1.0".to_string()),
            }),
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec![
                        ".".to_string(), 
                        "|".to_string(), 
                        "?".to_string(),
                        " ".to_string()
                    ]),
                    work_done_progress_options: Default::default(),
                    all_commit_characters: None,
                    completion_item: None,
                }),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                document_formatting_provider: Some(OneOf::Left(true)),
                code_action_provider: Some(CodeActionProviderCapability::Simple(true)),
                ..Default::default()
            },
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "jq-lsp server initialized!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let text = params.text_document.text;
        
        // Store document
        self.document_map.write().await.insert(uri.clone(), text.clone());
        
        // Send diagnostics using convenience function (which creates its own server instance)
        let diagnostics = convenience::get_diagnostics(&text);
        let lsp_diagnostics: Vec<Diagnostic> = diagnostics
            .iter()
            .map(|d| Self::diagnostic_to_lsp(d, &text))
            .collect();
            
        self.client
            .publish_diagnostics(uri, lsp_diagnostics, None)
            .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        
        if let Some(change) = params.content_changes.into_iter().next() {
            let text = change.text;
            
            // Update stored document
            self.document_map.write().await.insert(uri.clone(), text.clone());
            
            // Send updated diagnostics using convenience function
            let diagnostics = convenience::get_diagnostics(&text);
            let lsp_diagnostics: Vec<Diagnostic> = diagnostics
                .iter()
                .map(|d| Self::diagnostic_to_lsp(d, &text))
                .collect();
                
            self.client
                .publish_diagnostics(uri, lsp_diagnostics, None)
                .await;
        }
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        
        let document_map = self.document_map.read().await;
        if let Some(text) = document_map.get(&uri) {
            // Use convenience function for completion
            let completions = convenience::get_completions(text, position.line, position.character);
            
            let lsp_completions: Vec<CompletionItem> = completions
                .iter()
                .map(|item| Self::completion_item_to_lsp(item, text))
                .collect();
                
            return Ok(Some(CompletionResponse::Array(lsp_completions)));
        }
        
        Ok(None)
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        
        let document_map = self.document_map.read().await;
        if let Some(text) = document_map.get(&uri) {
            // Use convenience function for hover
            if let Some(hover_info) = convenience::get_hover(text, position.line, position.character) {
                let range = Self::text_range_to_lsp_range(text, hover_info.range);
                
                return Ok(Some(Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: hover_info.contents,
                    }),
                    range: Some(range),
                }));
            }
        }
        
        Ok(None)
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        let uri = params.text_document.uri;
        
        let document_map = self.document_map.read().await;
        if let Some(text) = document_map.get(&uri) {
            // Use convenience function for formatting
            if let Some(formatted) = convenience::format_jq(text) {
                let range = Range {
                    start: Position { line: 0, character: 0 },
                    end: offset_to_lsp_position(text, text.len()),
                };
                
                return Ok(Some(vec![TextEdit {
                    range,
                    new_text: formatted,
                }]));
            }
        }
        
        Ok(None)
    }

    async fn code_action(&self, params: CodeActionParams) -> Result<Option<CodeActionResponse>> {
        let uri = params.text_document.uri;
        
        let document_map = self.document_map.read().await;
        if let Some(text) = document_map.get(&uri) {
            let start_offset = position_to_offset(text, params.range.start.line, params.range.start.character);
            let end_offset = position_to_offset(text, params.range.end.line, params.range.end.character);
            
            // Get quick fixes using the server instance
            let mut server = self.server.write().await;
            let quick_fixes = server.quick_fixes(text, start_offset..end_offset);
            
            let mut actions = Vec::new();
            for fix in quick_fixes {
                let mut changes = HashMap::new();
                let text_edits: Vec<TextEdit> = fix.edits
                    .into_iter()
                    .map(|edit| TextEdit {
                        range: Self::text_range_to_lsp_range(text, edit.range),
                        new_text: edit.new_text,
                    })
                    .collect();
                
                changes.insert(uri.clone(), text_edits);
                
                actions.push(CodeActionOrCommand::CodeAction(CodeAction {
                    title: fix.title,
                    kind: Some(CodeActionKind::QUICKFIX),
                    edit: Some(WorkspaceEdit {
                        changes: Some(changes),
                        ..Default::default()
                    }),
                    ..Default::default()
                }));
            }
            
            if !actions.is_empty() {
                return Ok(Some(actions));
            }
        }
        
        Ok(None)
    }
}

/// Helper function to convert byte offset to line/column
fn offset_to_position(text: &str, offset: usize) -> (u32, u32) {
    let mut line = 0;
    let mut character = 0;
    
    for (i, ch) in text.char_indices() {
        if i >= offset {
            break;
        }
        if ch == '\n' {
            line += 1;
            character = 0;
        } else {
            character += 1;
        }
    }
    
    (line, character)
}

/// Helper to convert offset to LSP Position
fn offset_to_lsp_position(text: &str, offset: usize) -> Position {
    let (line, character) = offset_to_position(text, offset);
    Position { line, character }
}

/// Helper to convert LSP position to byte offset
fn position_to_offset(text: &str, line: u32, character: u32) -> usize {
    let mut current_line = 0;
    let mut current_character = 0;
    
    for (offset, ch) in text.char_indices() {
        if current_line == line && current_character == character {
            return offset;
        }
        
        if ch == '\n' {
            current_line += 1;
            current_character = 0;
        } else {
            current_character += 1;
        }
    }
    
    text.len()
}

#[tokio::main]
async fn main() {
    // tracing_subscriber::fmt().init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| JqLsp::new(client));
    Server::new(stdin, stdout, socket).serve(service).await;
}