use std::fmt::{Debug, Display};

#[derive(Default, Clone, PartialEq, Copy)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

impl Span {
    pub fn join(self, other: Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

impl Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}

#[derive(Default, Clone, PartialEq, Copy, Eq)]
pub struct Position {
    pub col: usize,
    pub row: usize,
}

impl Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.row, self.col)
    }
}

impl Debug for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}

impl Ord for Position {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        if self.row != other.row {
            self.row.cmp(&other.row)
        } else {
            self.col.cmp(&other.col)
        }
    }
}

impl PartialOrd for Position {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

pub struct Spanned<T> {
    pub span: Span,
    pub inner: T,
}

pub trait WithSpan {
    fn with_span(self, span: Span) -> Spanned<Self>
    where
        Self: Sized,
    {
        Spanned { inner: self, span }
    }
}

impl<T> WithSpan for T {}

impl<T: Display> Display for Spanned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.inner.fmt(f)?;
        write!(
            f,
            " on line {}, column {}",
            self.span.start.row, self.span.start.col
        )
    }
}
