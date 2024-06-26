use crate::{join_comma, span::Spanned, RODEO};
use lasso::Spur;

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Error,
    Primitive(Primitive),
    Function {
        parameters: Spanned<Vec<Spanned<Type>>>,
        return_ty: Spanned<Box<Type>>,
    },
    UserDefined(Spur),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Primitive {
    Integer,
    Float,
    Boolean,
}

impl core::fmt::Display for Type {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Error => write!(f, "<error>"),
            Self::Primitive(primitive) => write!(f, "{primitive}"),
            Self::Function {
                parameters,
                return_ty,
            } => {
                write!(
                    f,
                    "({}) -> {}",
                    join_comma(parameters.0.iter().map(|arg| &arg.0)),
                    return_ty.0
                )
            }
            Self::UserDefined(name) => write!(f, "{}", RODEO.resolve(name)),
        }
    }
}

impl core::fmt::Display for Primitive {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Integer => write!(f, "int"),
            Self::Float => write!(f, "float"),
            Self::Boolean => write!(f, "bool"),
        }
    }
}
