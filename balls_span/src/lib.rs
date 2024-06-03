use chumsky::span::{SimpleSpan, Span as _};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Ctx(pub usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Span(SimpleSpan<usize, Ctx>);

impl Default for Span {
    fn default() -> Self {
        Self(SimpleSpan::<usize, Ctx>::new(Ctx(0), 0..0))
    }
}

impl Span {
    #[must_use]
    pub fn new(context: Ctx, range: core::ops::Range<usize>) -> Self {
        Self(SimpleSpan::<usize, Ctx>::new(context, range))
    }
}

impl chumsky::span::Span for Span {
    type Context = Ctx;

    type Offset = usize;

    fn new(context: Self::Context, range: core::ops::Range<Self::Offset>) -> Self {
        Self::new(context, range)
    }

    fn context(&self) -> Self::Context {
        self.0.context()
    }

    fn start(&self) -> Self::Offset {
        self.0.start()
    }

    fn end(&self) -> Self::Offset {
        self.0.end()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Spanned<T>(pub T, pub Span);

impl<T> Spanned<T> {
    pub const fn as_ref(&self) -> Spanned<&T> {
        Spanned(&self.0, self.1)
    }

    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Spanned<U> {
        Spanned(f(self.0), self.1)
    }

    #[must_use]
    pub fn map_span<F: FnOnce(Span) -> Span>(self, f: F) -> Self {
        Self(self.0, f(self.1))
    }

    pub fn map_with_span<U, F: FnOnce(T, Span) -> U>(self, f: F) -> Spanned<U> {
        Spanned(f(self.0, self.1), self.1)
    }

    pub fn boxed(self) -> Spanned<Box<T>> {
        Spanned(Box::new(self.0), self.1)
    }
}

impl<T> Spanned<Box<T>> {
    #[must_use]
    pub fn unbox(self) -> Spanned<T> {
        Spanned(*self.0, self.1)
    }
}
