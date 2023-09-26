use std::{fmt, fs::File, io::Read, path::Path};

use codegen::IrError;

use crate::{
    codegen::Ir,
    pass::{SymbolTable, TypeCheckPass},
};

pub use compile::Compile;

mod ast;
mod codegen;
mod compile;
mod parse;
mod pass;
mod reporting;
pub mod subprocess;
mod token;

#[derive(thiserror::Error, Debug)]
pub enum PipelineError {
    #[error("file \"{0}\" does not exist")]
    FileNotFound(String),
    #[error("File is not valid UTF-8")]
    InvalidUtf8,
    #[error("(while lexing source) {0} error(s) encountered")]
    LexError(usize),
    #[error("(while parsing) {0} error(s) encountered")]
    ParseError(usize),
    #[error("(while type checking) {0} error(s) encountered")]
    TypeError(usize),
    #[error("(while building ir) {0}")]
    IrError(IrError),
    #[error("failed to compile (see output)")]
    CompileError,
}

#[derive(Debug)]
pub struct Program {
    ast: ast::Ast,
    ir: String,
}

impl Program {
    pub fn from_file<P>(path: P) -> Result<Self, PipelineError>
    where
        P: AsRef<Path>,
    {
        let source = Source::new(path)?;
        let ast = ast::Ast::from_source(source)?;
        Self::new(ast)?.build()
    }

    pub fn from_string(str: String) -> Result<Self, PipelineError> {
        let dir = Path::new("/tmp/com.github.alaidriel/kyanite");
        if !dir.exists() {
            std::fs::create_dir_all(dir).expect("permission to write to /tmp");
        }
        let filename = dir
            .join("main.kya")
            .to_str()
            .ok_or(PipelineError::InvalidUtf8)?
            .to_string();
        std::fs::write(
            &filename,
            format!("defn main(): int {{\n\t{}\nreturn 0;\n}}", str),
        )
        .map_err(|_| PipelineError::FileNotFound(filename.clone()))?;
        let ast = ast::Ast::from_source(Source::new(filename)?)?;
        Self::new(ast)?.build()
    }

    fn new(ast: ast::Ast) -> Result<Self, PipelineError> {
        Ok(Self {
            ir: Ir::from_ast(&ast).map_err(PipelineError::IrError)?,
            ast,
        })
    }

    fn build(self) -> Result<Self, PipelineError> {
        let symbols = SymbolTable::from(&self.ast.file);
        let mut pass = TypeCheckPass::new(symbols, &self.ast.file);
        let errors = pass.run();
        if errors > 0 {
            return Err(PipelineError::TypeError(errors));
        }
        Ok(self)
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.ast.file)
    }
}

#[derive(Debug, Default)]
pub struct Source {
    filename: String,
    chars: Vec<char>,
    raw: String,
}

impl Source {
    pub fn new<P>(path: P) -> Result<Self, PipelineError>
    where
        P: AsRef<Path>,
    {
        let filename = path
            .as_ref()
            .to_str()
            .ok_or(PipelineError::InvalidUtf8)?
            .to_string();
        let mut raw = String::new();
        let mut file =
            File::open(&path).map_err(|_| PipelineError::FileNotFound(filename.clone()))?;
        file.read_to_string(&mut raw)
            .map_err(|_| PipelineError::InvalidUtf8)?;
        Ok(Self {
            filename,
            chars: raw.chars().collect(),
            raw,
        })
    }
}
