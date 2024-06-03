use super::Diag;
use codespan_reporting::diagnostic::Severity;

#[derive(Clone, Debug)]
#[allow(missing_copy_implementations)] // todo: remove this
pub enum Warning {}

impl Diag for Warning {
    fn message(&self) -> String {
        todo!()
    }

    fn spans(&self) -> Vec<super::ErrorSpan> {
        todo!()
    }

    fn notes(&self) -> Vec<String> {
        todo!()
    }

    fn kind(&self) -> Severity {
        Severity::Warning
    }
}
