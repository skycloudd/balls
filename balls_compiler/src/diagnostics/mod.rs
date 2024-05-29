pub use error::Error;
pub use warning::Warning;

pub mod error;
pub mod warning;

#[derive(Debug)]
pub struct Diagnostics {
    warnings: Vec<Warning>,
    errors: Vec<Error>,
}

impl Diagnostics {
    pub fn new() -> Self {
        Self {
            warnings: Vec::new(),
            errors: Vec::new(),
        }
    }

    pub fn warnings(&self) -> &[Warning] {
        &self.warnings
    }

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
