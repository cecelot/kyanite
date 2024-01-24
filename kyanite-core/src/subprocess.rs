use colored::Colorize;
use std::io::Write;

use crate::PipelineError;

pub struct ProcessResult {
    pub command: String,
    pub code: i32,
    pub output: String,
}

#[must_use]
pub fn exec(cmd: &str, args: &[&str]) -> ProcessResult {
    let output = std::process::Command::new(cmd).args(args).output().unwrap();
    let code = output.status.code().unwrap_or(0);
    let command = format!("{} {}", cmd, args.join(" "));
    ProcessResult {
        code,
        command,
        output: std::str::from_utf8(&output.stdout).unwrap().to_owned()
            + std::str::from_utf8(&output.stderr).unwrap(),
    }
}

pub fn handle(verb: &str, res: ProcessResult, mut writer: impl Write) -> Result<(), PipelineError> {
    if res.code == 0 {
        writeln!(writer, "{} `{}`", verb.green().bold(), res.command).unwrap();
        Ok(())
    } else {
        writeln!(
            writer,
            "{} `{}`\n\t{}",
            "Failed".red().bold(),
            res.command,
            res.output
        )
        .unwrap();
        Err(PipelineError::CompileError(res.output))
    }
}
