use std::time::Duration;

#[derive(Debug)]
pub struct ProcessResult {
    pub command: String,
    pub code: i32,
    pub output: String,
    pub elapsed: Duration,
}

#[must_use]
pub fn exec(cmd: &str, args: &[&str]) -> ProcessResult {
    let start = std::time::Instant::now();
    let output = std::process::Command::new(cmd).args(args).output().unwrap();
    let elapsed = start.elapsed();
    let code = output.status.code().unwrap_or(0);
    let command = format!("{} {}", cmd, args.join(" "));
    ProcessResult {
        code,
        command,
        elapsed,
        output: std::str::from_utf8(&output.stdout).unwrap().to_owned()
            + std::str::from_utf8(&output.stderr).unwrap(),
    }
}

pub fn handle(res: ProcessResult) -> Result<(), String> {
    if res.code == 0 {
        log::info!(
            "finished `{}` in {}ms",
            res.command.split_once(' ').unwrap().0,
            res.elapsed.as_millis()
        );
        Ok(())
    } else {
        log::error!("while running `{}`\n\t{}", res.command, res.output);
        Err(res.output)
    }
}
