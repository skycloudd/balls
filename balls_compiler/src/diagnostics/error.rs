use super::{Diag, ErrorSpan};
use crate::{
    parser::ast::{BinaryOp, UnaryOp},
    span::{Span, Spanned},
    typecheck::types,
    RODEO,
};
use chumsky::error::{Rich, RichReason};
use codespan_reporting::diagnostic::Severity;
use lasso::Spur;

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
        ident: Spur,
        span: Span,
    },
    UnknownType {
        span: Span,
    },
    CannotBinaryOp {
        lhs_ty: Spanned<types::Type>,
        rhs_ty: Spanned<types::Type>,
        op: Spanned<BinaryOp>,
        span: Span,
    },
    CannotUnaryOp {
        expr_ty: Spanned<types::Type>,
        op: Spanned<UnaryOp>,
        span: Span,
    },
    TypeMismatch {
        expected: Spanned<types::Type>,
        found: Spanned<types::Type>,
    },
    ArgumentCountMismatch {
        expected: usize,
        found: usize,
        expected_span: Span,
        found_span: Span,
    },
    CannotCall {
        ty: Spanned<types::Type>,
        span: Span,
    },
    FeatureNotImplemented {
        feature: &'static str,
        span: Span,
    },
    MainFunctionHasParameters {
        span: Span,
    },
    MainFunctionReturnType {
        span: Span,
    },
}

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
                format!("Undefined name '{}'", RODEO.resolve(ident))
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
            Self::ArgumentCountMismatch {
                expected,
                found,
                expected_span: _,
                found_span: _,
            } => format!("Expected {expected} arguments, found {found}"),
            Self::CannotCall { ty, span: _ } => {
                format!("Cannot call value of type '{}'", ty.0)
            }
            Self::FeatureNotImplemented { feature, span: _ } => {
                format!("{feature} is not yet implemented")
            }
            Self::MainFunctionHasParameters { span: _ } => {
                "Main function cannot have parameters".to_string()
            }
            Self::MainFunctionReturnType { span: _ } => {
                "Main function must return an integer".to_string()
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
                Some(format!("undefined name '{}'", RODEO.resolve(ident))),
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
            Self::ArgumentCountMismatch {
                expected,
                found,
                expected_span,
                found_span,
            } => vec![
                ErrorSpan::Primary(
                    Some(format!("function takes {expected} parameters")),
                    *expected_span,
                ),
                ErrorSpan::Primary(Some(format!("found {found} arguments")), *found_span),
            ],
            Self::CannotCall { ty, span } => {
                vec![ErrorSpan::Primary(Some(format!("{}", ty.0)), *span)]
            }
            Self::FeatureNotImplemented { feature: _, span }
            | Self::MainFunctionHasParameters { span }
            | Self::MainFunctionReturnType { span } => {
                vec![ErrorSpan::Primary(None, *span)]
            }
        }
    }

    fn notes(&self) -> Vec<String> {
        match self {
            Self::FeatureNotImplemented {
                feature: _,
                span: _,
            } => {
                vec!["This feature has not yet been implemented. It will be added in a future version.".to_string()]
            }
            Self::MainFunctionHasParameters { span: _ } => vec![
                "The main function is the entry point of the program and cannot have parameters."
                    .to_string(),
            ],
            Self::MainFunctionReturnType { span: _ } => vec![
                "The main function is the entry point of the program and must return an integer as exit code."
                    .to_string(),
                "Add `-> int` to the function signature.".to_string(),
            ],
            _ => vec![],
        }
    }

    fn kind(&self) -> Severity {
        Severity::Error
    }
}

#[must_use]
pub fn convert(error: &Rich<String, Span, &str>) -> Vec<Error> {
    fn convert_inner(reason: &RichReason<String, &str>, span: Span) -> Vec<Error> {
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
