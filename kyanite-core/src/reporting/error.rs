use crate::{token::Span, Source};
use colored::Colorize;
use std::fmt;

#[derive(Debug)]
pub struct PreciseError<'a> {
    filename: &'a str,
    heading: String,
    source: String,
    span: Span,
    text: String,
}

impl<'a> PreciseError<'a> {
    pub fn new(source: &Source, span: Span, heading: String, text: String) -> Self {
        Self {
            source: source
                .raw
                .lines()
                .nth(span.line - 1)
                .expect("span should have valid line number")
                .into(),
            filename: source.filename,
            span,
            heading,
            text,
        }
    }

    fn aligned<F>(&self, comment: &mut String, rest: F)
    where
        F: FnOnce(&mut String),
    {
        let num = self.span.line.to_string();
        comment.push_str(&format!(
            "{}{}",
            " ".repeat(num.len() + 1),
            "|".blue().bold()
        ));
        rest(comment);
        comment.push('\n');
    }

    fn build(&self) -> String {
        let num = self.span.line.to_string();
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

impl fmt::Display for PreciseError<'_> {
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
