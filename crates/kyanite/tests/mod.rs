mod kyir;
mod llvm;

pub fn path(name: &str) -> Result<String, Box<dyn std::error::Error>> {
    let dir = {
        let mut dir = std::env::current_dir()?;
        dir.pop();
        dir.pop();
        dir.push("examples");
        dir.to_string_lossy().to_string()
    };
    Ok(format!("{dir}/{name}"))
}
