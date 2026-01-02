use tower_lsp::{LspService, Server};
use tracing_subscriber::EnvFilter;
mod hover;
mod assists;
mod document;
mod builtins;
mod completions;
mod server;

#[cfg(test)]
mod tests;

use server::TjqLanguageServer;

#[tokio::main]
async fn main() {
    tracing_subscriber::fmt()
        .with_env_filter(
            EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| EnvFilter::new("tjq_lsp=debug,tower_lsp=debug"))
        )
        .with_writer(std::io::stderr)
        .init();

    tracing::info!("Starting TJQ Language Server");

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(TjqLanguageServer::new);
    
    Server::new(stdin, stdout, socket)
        .serve(service)
        .await;
}
