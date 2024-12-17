use kabanos_common::ast::parser::Parser;
use kabanos_common::lexer::Lexer;
use kabanos_common::semantic::Module;
use kabanos_common::span::Span;
use tokio::sync::Mutex;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

#[derive(Debug)]
struct Backend {
    client: Client,
    text: Mutex<String>,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        let capabilities = ServerCapabilities {
            position_encoding: Some(PositionEncodingKind::UTF8),
            hover_provider: Some(HoverProviderCapability::Simple(true)),
            text_document_sync: Some(TextDocumentSyncKind::FULL.into()),
            ..Default::default()
        };
        let server_info = Some(ServerInfo {
            name: "kabanos_lsp".to_string(),
            version: Some("0".to_string()),
        });
        Ok(InitializeResult {
            capabilities,
            server_info,
        })
    }

    async fn hover(&self, _params: HoverParams) -> Result<Option<Hover>> {
        let hover = Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: "nihao".to_string(),
            }),
            range: None,
        };

        Ok(Some(hover))
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .show_message(MessageType::INFO, "server initialized!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let text = params.text_document.text;
        let uri = params.text_document.uri;
        let version = params.text_document.version;

        let mut guard = self.text.lock().await;
        *guard = text;
        drop(guard);
        self.diagnose(uri, version).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let mut guard = self.text.lock().await;
        for change in params.content_changes {
            *guard = change.text;
        }
        drop(guard);
        self.diagnose(params.text_document.uri, params.text_document.version)
            .await;
    }
}

impl Backend {
    async fn diagnose(&self, uri: Url, version: i32) {
        let text = self.text.lock().await;

        let lexer = Lexer::new(text.chars());
        let parser = Parser::new(lexer);
        let Some(mut parser) = parser else {
            return;
        };

        let mut diagnostics = Vec::new();

        match parser.module() {
            Err(err) => {
                let range = span_to_range(err.get_span());
                let diag = Diagnostic {
                    range,
                    message: format!("{}", err),
                    ..Default::default()
                };
                diagnostics.push(diag);
            }
            Ok(module) => {
                if let Err(err) = Module::build_module(module) {
                    let range = span_to_range(err.span);
                    let diag = Diagnostic {
                        range,
                        message: format!("{}", err),
                        ..Default::default()
                    };
                    diagnostics.push(diag);
                }
            }
        };

        self.client
            .publish_diagnostics(uri, diagnostics, Some(version))
            .await;
    }
}

fn span_to_range(span: Span) -> Range {
    Range {
        start: Position {
            line: span.start.row as u32,
            character: span.start.col as u32,
        },
        end: Position {
            line: span.end.row as u32,
            character: span.end.col as u32,
        },
    }
}

#[tokio::main]
async fn main() {
    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::INFO)
        .with_writer(std::io::stderr)
        .init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend {
        client,
        text: Mutex::default(),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
