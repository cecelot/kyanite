use crate::{ast::Ast, Source};

const PRELUDE: &str = indoc::indoc! {"
extern fun println_str(s: str)
extern fun println_int(i: int)
extern fun println_float(f: float)
extern fun println_bool(b: bool)

extern fun max_int(a: int, b: int): int
extern fun min_int(a: int, b: int): int

extern fun max_float(a: float, b: float): float
extern fun min_float(a: float, b: float): float
"};

pub fn builtins() -> Ast {
    let source = Source::in_memory(PRELUDE.to_string());
    Ast::try_from(&source).unwrap()
}
