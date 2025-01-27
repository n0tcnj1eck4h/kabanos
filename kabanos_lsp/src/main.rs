use std::fmt::Display;

use kabanos_common::ast::parser::Parser;
use kabanos_common::lexer::Lexer;
use kabanos_common::semantic::Module;
use kabanos_common::span::{HasSpan, Span};
use tokio::sync::Mutex;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

#[derive(Debug)]
struct KabanosLSPBackend {
    client: Client,
    code: Mutex<String>,
}

impl KabanosLSPBackend {
    fn new(client: Client) -> Self {
        Self {
            client,
            code: Mutex::default(),
        }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for KabanosLSPBackend {
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

        let mut guard = self.code.lock().await;
        *guard = text;
        drop(guard);
        self.diagnose(uri, version).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let mut guard = self.code.lock().await;
        for change in params.content_changes {
            *guard = change.text;
        }
        drop(guard);
        self.diagnose(params.text_document.uri, params.text_document.version)
            .await;
    }
}

impl KabanosLSPBackend {
    async fn diagnose(&self, uri: Url, version: i32) {
        let text = self.code.lock().await;
        let mut diagnostics = Vec::new();

        let lexer = Lexer::new(text.chars());
        let tokens = lexer.filter_map(|t| match t {
            Ok(t) => Some(t),
            Err(err) => {
                diagnostics.push(err_to_diagnostic(err));
                None
            }
        });

        let parser = Parser::new(tokens);
        let Some(mut parser) = parser else {
            return;
        };

        match parser.module() {
            Err(err) => {
                diagnostics.push(err_to_diagnostic(err));
            }
            Ok(module) => {
                if let Err(errs) = Module::build_module(module) {
                    diagnostics.extend(errs.into_iter().map(err_to_diagnostic));
                }
            }
        };

        self.client
            .publish_diagnostics(uri, diagnostics, Some(version))
            .await;
    }
}

fn err_to_diagnostic(err: impl Display + HasSpan) -> Diagnostic {
    let range = span_to_range(err.get_span());
    Diagnostic {
        range,
        message: format!("{}", err),
        severity: Some(DiagnosticSeverity::ERROR),
        ..Default::default()
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

    let (service, socket) = LspService::new(KabanosLSPBackend::new);
    Server::new(stdin, stdout, socket).serve(service).await;
}
