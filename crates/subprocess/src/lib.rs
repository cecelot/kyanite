#[derive(Debug)]
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

pub fn handle(res: ProcessResult) -> Result<(), String> {
    if res.code == 0 {
        log::info!("completed `{}`", res.command);
        Ok(())
    } else {
        log::error!("while running `{}`\n\t{}", res.command, res.output);
        Err(res.output)
    }
}
