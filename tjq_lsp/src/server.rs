use dashmap::DashMap;
use ropey::Rope;
use std::sync::Arc;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::lsp_types::CodeActionOrCommand;
use tower_lsp::{Client, LanguageServer};
use tree_sitter::{Parser, Tree};


use crate::document::DocumentData;
use crate::completions::CompletionProvider;
use crate::hover::HoverProvider;
use crate::assists::AssistProvider;


pub struct TjqLanguageServer {
    client: Client,
    documents: Arc<DashMap<Url, DocumentData>>,
    parser: Arc<tokio::sync::Mutex<Parser>>,
}

impl TjqLanguageServer {
    pub fn new(client: Client) -> Self {
        let mut parser = Parser::new();
        parser
            .set_language(&tree_sitter_tjq::LANGUAGE.into())
            .expect("Failed to set TJQ language");

        Self {
            client,
            documents: Arc::new(DashMap::new()),
            parser: Arc::new(tokio::sync::Mutex::new(parser)),
        }
    }

    async fn parse_document(&self, uri: &Url, text: &str) -> Option<Tree> {
        let mut parser = self.parser.lock().await;
        parser.parse(text, None)
    }

    async fn update_document(&self, uri: Url, text: String, version: i32) {
        let rope = Rope::from_str(&text);
        let tree = self.parse_document(&uri, &text).await;
        
        let doc_data = DocumentData {
            rope,
            version,
            tree,
            text,
        };
        
        self.documents.insert(uri, doc_data);
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for TjqLanguageServer {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec![
                        ".".to_string(),
                        "[".to_string(),
                        "(".to_string(),
                        "{".to_string(),
                        "|".to_string(),
                        "$".to_string(),
                    ]),
                    ..Default::default()
                }),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                code_action_provider: Some(CodeActionProviderCapability::Simple(true)),
                document_formatting_provider: Some(OneOf::Left(true)),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        tracing::info!("TJQ Language Server initialized");
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let text = params.text_document.text;
        let version = params.text_document.version;
        
        self.update_document(uri.clone(), text, version).await;
        tracing::debug!("Document opened: {}", uri);
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        let version = params.text_document.version;
        
        if let Some(change) = params.content_changes.into_iter().next() {
            self.update_document(uri.clone(), change.text, version).await;
            tracing::debug!("Document changed: {}", uri);
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        self.documents.remove(&params.text_document.uri);
        tracing::debug!("Document closed: {}", params.text_document.uri);
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        
        let completions = if let Some(doc) = self.documents.get(&uri) {
            CompletionProvider::provide_completions(&doc, position, params.context)
        } else {
            vec![]
        };
        
        Ok(Some(CompletionResponse::Array(completions)))
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        
        if let Some(doc) = self.documents.get(&uri) {
            Ok(HoverProvider::provide_hover(&doc, position))
        } else {
            Ok(None)
        }
    }

    async fn code_action(&self, params: CodeActionParams) -> Result<Option<CodeActionResponse>> {
        let uri = params.text_document.uri;
        let range = params.range;
        
        let actions = if let Some(doc) = self.documents.get(&uri) {
            AssistProvider::provide_assists(&doc, range, params.context, uri)
        } else {
            vec![]
        };
        
        let response: Vec<CodeActionOrCommand> = actions
            .into_iter()
            .map(CodeActionOrCommand::CodeAction)
            .collect();
        
        Ok(Some(response))
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        let uri = params.text_document.uri;
        
        if let Some(doc) = self.documents.get(&uri) {
            let text = &doc.text;
            let formatted = format_tjq_code(text);
            
            if formatted != *text {
                let range = Range::new(
                    Position::new(0, 0),
                    Position::new(doc.rope.len_lines() as u32, 0),
                );
                
                Ok(Some(vec![TextEdit {
                    range,
                    new_text: formatted,
                }]))
            } else {
                Ok(None)
            }
        } else {
            Ok(None)
        }
    }
}

pub fn format_tjq_code(code: &str) -> String {
    let mut result = String::new();
    let mut chars = code.chars().peekable();
    
    while let Some(ch) = chars.next() {
        match ch {
            '|' | ',' | '+' | '-' | '*' | '/' | '%' | '=' => {
                // Add space before if not already present
                if !result.ends_with(' ') && !result.ends_with('\n') && !result.is_empty() {
                    result.push(' ');
                }
                result.push(ch);
                // Add space after if next char is not space or newline
                if let Some(&next) = chars.peek() {
                    if next != ' ' && next != '\n' && next != '=' {
                        result.push(' ');
                    }
                }
            }
            _ => result.push(ch),
        }
    }
    
    result
}
