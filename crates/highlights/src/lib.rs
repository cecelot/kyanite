#[macro_use]
extern crate napi_derive;

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
    pub kind: String,
    pub modifiers: Vec<String>,
}

impl Highlight {
    pub fn single(
        start: usize,
        end: usize,
        line: usize,
        kind: String,
        modifiers: Vec<String>,
    ) -> Self {
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
    vec![Highlight::single(0, 3, 0, "keyword".to_string(), vec![])]
}
