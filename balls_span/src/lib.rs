use chumsky::span::{SimpleSpan, Span as _};

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct Ctx(pub usize);

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct Span(SimpleSpan<usize, Ctx>);

impl Span {
    #[must_use]
    pub fn new(context: Ctx, range: core::ops::Range<usize>) -> Self {
        Self(SimpleSpan::<usize, Ctx>::new(context, range))
    }

    #[must_use]
    pub fn union(&self, other: &Self) -> Self {
        Self(self.0.union(other.0))
    }

    #[must_use]
    pub fn end_span(&self) -> Self {
        Self(self.0.to_end())
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

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct Spanned<T>(pub T, pub Span);

impl<T> Spanned<T> {
    pub const fn as_ref(&self) -> Spanned<&T> {
        Spanned(&self.0, self.1)
    }

    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Spanned<U> {
        Spanned(f(self.0), self.1)
    }

    pub fn boxed(self) -> Spanned<Box<T>> {
        Spanned(Box::new(self.0), self.1)
    }
}

impl<T> core::ops::Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> core::ops::DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
