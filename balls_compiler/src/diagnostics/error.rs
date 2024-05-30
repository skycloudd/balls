use super::{Diag, ErrorSpan};
use balls_span::Span;
use chumsky::error::{Rich, RichReason};
use codespan_reporting::diagnostic::Severity;

#[derive(Clone, Debug)]
pub enum Error {
    ExpectedFound {
        expected: Vec<String>,
        found: Option<String>,
        span: Span,
    },
    Custom {
        message: String,
        span: Span,
    },
}

#[allow(clippy::match_same_arms)]
impl Diag for Error {
    fn message(&self) -> String {
        match self {
            Self::ExpectedFound {
                expected,
                found,
                span: _,
            } => format!(
                "Expected one of {}, found {}",
                expected.join(", "),
                found.as_deref().unwrap_or("nothing")
            ),
            Self::Custom { message, span: _ } => message.clone(),
        }
    }

    fn spans(&self) -> Vec<ErrorSpan> {
        match self {
            Self::ExpectedFound {
                expected,
                found: _,
                span,
            } => vec![ErrorSpan::Primary(
                Some(format!("expected one of {}", expected.join(", "))),
                *span,
            )],
            Self::Custom { message: _, span } => vec![ErrorSpan::Primary(None, *span)],
        }
    }

    fn notes(&self) -> Vec<String> {
        match self {
            Self::ExpectedFound {
                expected: _,
                found: _,
                span: _,
            } => vec![],
            Self::Custom {
                message: _,
                span: _,
            } => vec![],
        }
    }

    fn kind(&self) -> Severity {
        Severity::Error
    }
}

#[must_use]
pub fn convert(error: &Rich<'_, String, Span>) -> Vec<Error> {
    fn convert_inner(reason: &RichReason<String>, span: Span) -> Vec<Error> {
        match reason {
            RichReason::ExpectedFound { expected, found } => vec![Error::ExpectedFound {
                expected: expected.iter().map(ToString::to_string).collect(),
                found: found.as_ref().map(|f| f.to_string()),
                span,
            }],
            RichReason::Custom(message) => vec![Error::Custom {
                message: message.clone(),
                span,
            }],
            RichReason::Many(reasons) => reasons
                .iter()
                .flat_map(|r| convert_inner(r, span))
                .collect(),
        }
    }

    convert_inner(error.reason(), *error.span())
}
