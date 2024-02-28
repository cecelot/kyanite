#[macro_use]
extern crate napi_derive;

mod kind;

use kind::Kind;

#[napi(object)]
pub struct Span {
    pub start: u32,
    pub end: u32,
    pub line: u32,
}

impl Span {
    pub fn new(start: usize, end: usize, line: usize) -> Self {
        Self {
            start: u32::try_from(start).unwrap(),
            end: u32::try_from(end).unwrap(),
            line: u32::try_from(line).unwrap(),
        }
    }
}

#[napi(object)]
pub struct MultiSpan {
    pub start: Span,
    pub end: Span,
}

impl MultiSpan {
    pub fn new(start: Span, end: Span) -> Self {
        Self { start, end }
    }
}

#[napi(object)]
pub struct Highlight {
    pub span: Span,
    pub kind: Kind,
    pub modifiers: Vec<String>,
}

impl Highlight {
    pub fn new(start: usize, end: usize, line: usize, kind: Kind, modifiers: Vec<String>) -> Self {
        Self {
            span: Span::new(start, end, line),
            kind,
            modifiers,
        }
    }
}

#[napi(object)]
#[derive(Debug)]
pub struct Source {
    pub code: String,
    pub filename: String,
}

#[napi]
pub fn highlights(_source: Source) -> Vec<Highlight> {
    vec![Highlight::new(0, 3, 0, Kind::Keyword, vec![])]
}
