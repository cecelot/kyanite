use std::fmt;

#[napi(string_enum)]
pub enum Kind {
    Keyword,
}

impl fmt::Display for Kind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Kind::Keyword => write!(f, "keyword"),
        }
    }
}
