use super::{Diag, ErrorSpan};
use crate::{
    parser::ast::{BinaryOp, UnaryOp},
    typecheck,
};
use balls_span::{Span, Spanned};
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
    UndefinedName {
        ident: &'static str,
        span: Span,
    },
    UnknownType {
        span: Span,
    },
    CannotBinaryOp {
        lhs_ty: Spanned<typecheck::types::Type>,
        rhs_ty: Spanned<typecheck::types::Type>,
        op: Spanned<BinaryOp>,
        span: Span,
    },
    CannotUnaryOp {
        expr_ty: Spanned<typecheck::types::Type>,
        op: Spanned<UnaryOp>,
        span: Span,
    },
    TypeMismatch {
        expected: Spanned<typecheck::types::Type>,
        found: Spanned<typecheck::types::Type>,
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
                found
                    .as_ref()
                    .map(|found| format!("'{found}'"))
                    .as_deref()
                    .unwrap_or("end of input")
            ),
            Self::Custom { message, span: _ } => message.clone(),
            Self::UndefinedName { ident, span: _ } => {
                format!("Undefined name '{ident}'")
            }
            Self::UnknownType { span: _ } => "Unknown type".to_string(),
            Self::CannotBinaryOp {
                lhs_ty,
                rhs_ty,
                op,
                span: _,
            } => {
                format!(
                    "Cannot apply binary operator '{}' to values of type '{}' and '{}'",
                    op.0, lhs_ty.0, rhs_ty.0
                )
            }
            Self::CannotUnaryOp {
                expr_ty,
                op,
                span: _,
            } => {
                format!(
                    "Cannot apply unary operator '{}' to value of type '{}'",
                    op.0, expr_ty.0
                )
            }
            Self::TypeMismatch { expected, found } => {
                format!("Expected type '{}', found '{}'", expected.0, found.0)
            }
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
            Self::UndefinedName { ident, span } => vec![ErrorSpan::Primary(
                Some(format!("undefined name '{ident}'")),
                *span,
            )],
            Self::UnknownType { span } => {
                vec![ErrorSpan::Primary(Some("unknown type".to_string()), *span)]
            }
            Self::CannotBinaryOp {
                lhs_ty,
                rhs_ty,
                op: _,
                span: _,
            } => vec![
                ErrorSpan::Primary(Some(format!("{}", lhs_ty.0)), lhs_ty.1),
                ErrorSpan::Primary(Some(format!("{}", rhs_ty.0)), rhs_ty.1),
            ],
            Self::CannotUnaryOp {
                expr_ty,
                op: _,
                span: _,
            } => vec![ErrorSpan::Primary(
                Some(format!("{}", expr_ty.0)),
                expr_ty.1,
            )],
            Self::TypeMismatch { expected, found } => {
                vec![
                    ErrorSpan::Primary(Some(format!("{}", expected.0)), expected.1),
                    ErrorSpan::Primary(Some(format!("{}", found.0)), found.1),
                ]
            }
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
            Self::UndefinedName { ident: _, span: _ } => vec![],
            Self::UnknownType { span: _ } => vec![],
            Self::CannotBinaryOp {
                lhs_ty: _,
                rhs_ty: _,
                op: _,
                span: _,
            } => vec![],
            Self::CannotUnaryOp {
                expr_ty: _,
                op: _,
                span: _,
            } => vec![],
            Self::TypeMismatch {
                expected: _,
                found: _,
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
