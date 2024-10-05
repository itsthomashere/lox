use std::{
    cmp::{max, min},
    fmt::Display,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
#[repr(transparent)]
pub struct BytePosition(pub u32);

impl BytePosition {
    pub fn shift(&mut self, ch: char) {
        self.0 += ch.len_utf8() as u32
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Span {
    pub start: BytePosition,
    pub end: BytePosition,
}

impl Span {
    pub fn new(start: u32, end: u32) -> Self {
        Self {
            start: BytePosition(start),
            end: BytePosition(end),
        }
    }

    pub const fn empty() -> Self {
        Self {
            start: BytePosition(0),
            end: BytePosition(0),
        }
    }

    pub fn union_span(a: Span, b: Span) -> Self {
        Self {
            start: min(a.start, b.start),
            end: max(a.end, b.end),
        }
    }

    pub fn union<A, B>(a: &WithSpan<A>, b: &WithSpan<B>) -> Self {
        Self::union_span(a.into(), b.into())
    }
}

impl<T> From<&WithSpan<T>> for Span {
    fn from(value: &WithSpan<T>) -> Self {
        value.span.clone()
    }
}

impl<T> From<WithSpan<T>> for Span {
    fn from(value: WithSpan<T>) -> Self {
        value.span
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct WithSpan<T> {
    pub val: T,
    pub span: Span,
}

impl<T> WithSpan<T> {
    pub fn new(val: T, span: Span) -> Self {
        Self { val, span }
    }

    pub const fn empty(val: T) -> Self {
        Self {
            val,
            span: Span::empty(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Diagnostic {
    pub span: Span,
    pub message: String,
}

pub struct LineOffset {
    offsets: Vec<u32>,
    len: u32,
}

impl LineOffset {
    pub fn new(source: &str) -> Self {
        let mut offsets = vec![0];
        let len = source.len() as u32;

        for (i, val) in source.bytes().enumerate() {
            if val == b'\n' {
                offsets.push((i + 1) as u32);
            }
        }

        Self { offsets, len }
    }

    pub fn line(&self, pos: BytePosition) -> usize {
        let offset = pos.0;
        assert!(offset <= self.len);

        match self.offsets.binary_search(&offset) {
            Ok(line) => line + 1,
            Err(line) => line,
        }
    }
}

impl<T> Display for WithSpan<T>
where
    T: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "<{}:{}> {}",
            self.span.start.0, self.span.end.0, self.val
        )
    }
}
