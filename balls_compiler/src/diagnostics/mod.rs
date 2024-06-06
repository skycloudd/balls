use chumsky::span::Span as _;
use codespan_reporting::diagnostic::{Diagnostic, Label, Severity};
pub use error::Error;
pub use warning::Warning;

use crate::span::Span;

pub mod error;
pub mod warning;

#[derive(Debug)]
pub struct Diagnostics {
    warnings: Vec<Warning>,
    errors: Vec<Error>,
}

impl Diagnostics {
    #[must_use]
    pub fn new() -> Self {
        Self {
            warnings: Vec::new(),
            errors: Vec::new(),
        }
    }

    #[must_use]
    pub fn warnings(&self) -> &[Warning] {
        &self.warnings
    }

    #[must_use]
    pub fn errors(&self) -> &[Error] {
        &self.errors
    }

    pub fn add_warning(&mut self, warning: Warning) {
        self.warnings.push(warning);
    }

    pub fn add_warnings(&mut self, warnings: impl IntoIterator<Item = Warning>) {
        self.warnings.extend(warnings);
    }

    pub fn add_error(&mut self, error: Error) {
        self.errors.push(error);
    }

    pub fn add_errors(&mut self, errors: impl IntoIterator<Item = Error>) {
        self.errors.extend(errors);
    }
}

impl Default for Diagnostics {
    fn default() -> Self {
        Self::new()
    }
}

pub trait Diag {
    fn message(&self) -> String;
    fn spans(&self) -> Vec<ErrorSpan>;
    fn notes(&self) -> Vec<String>;
    fn kind(&self) -> Severity;
}

#[derive(Debug)]
pub enum ErrorSpan {
    Primary(Option<String>, Span),
    Secondary(Option<String>, Span),
}

pub fn report(diagnostic: &impl Diag) -> Diagnostic<usize> {
    codespan_reporting::diagnostic::Diagnostic::new(diagnostic.kind())
        .with_message(diagnostic.message())
        .with_labels(
            diagnostic
                .spans()
                .into_iter()
                .map(|span| match span {
                    ErrorSpan::Primary(message, span) => {
                        let mut label = Label::primary(span.context().0, span.start()..span.end());

                        if let Some(message) = message {
                            label = label.with_message(message);
                        }

                        label
                    }
                    ErrorSpan::Secondary(message, span) => {
                        let mut label =
                            Label::secondary(span.context().0, span.start()..span.end());

                        if let Some(message) = message {
                            label = label.with_message(message);
                        }

                        label
                    }
                })
                .collect(),
        )
        .with_notes(diagnostic.notes())
}
