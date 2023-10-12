use colored::Colorize;
use serde::{Deserialize, Serialize};
use std::fmt;

use crate::{token::Span, Source};

#[derive(Debug, Serialize, Deserialize)]
pub struct PreciseError {
    filename: String,
    heading: String,
    source: String,
    span: Span,
    text: String,
}

impl PreciseError {
    pub fn new(source: &Source, span: Span, heading: String, text: String) -> Self {
        Self {
            source: source
                .raw
                .lines()
                .nth(span.line - 1)
                .expect("span to have valid line number")
                .into(),
            filename: source.filename.clone(),
            span,
            heading,
            text,
        }
    }

    fn aligned<F>(&self, comment: &mut String, rest: F)
    where
        F: FnOnce(&mut String),
    {
        let num: String = self.span.line.to_string();
        comment.push_str(&format!(
            "{}{}",
            " ".repeat(num.len() + 1),
            "|".blue().bold()
        ));
        rest(comment);
        comment.push('\n');
    }

    fn build(&self) -> String {
        let num: String = self.span.line.to_string();
        let mut comment = format!(
            "{}{}{}",
            &"-".repeat(num.len() + 1).blue().bold(),
            &">".blue().bold(),
            &format!(
                " {}:{}:{}\n",
                self.filename, self.span.line, self.span.column
            ),
        );
        self.aligned(&mut comment, |_| {});
        comment.push_str(&format!(
            "{} {} {}\n",
            self.span.line.to_string().blue().bold(),
            "|".blue().bold(),
            self.source
        ));
        self.aligned(&mut comment, |s| {
            s.push_str(&" ".repeat(self.span.column));
            s.push_str(
                &format!("{} {}", "^".repeat(self.span.length), self.text)
                    .red()
                    .bold()
                    .to_string(),
            );
        });
        self.aligned(&mut comment, |_| {});

        comment
    }
}

impl fmt::Display for PreciseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}: {}\n{}",
            "error".bold().red(),
            self.heading,
            self.build(),
        )
    }
}
