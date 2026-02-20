use std::{
    env, fs,
    path::{Path, PathBuf},
    process::Command,
};

use crate::{
    error::{fail, NoerResult},
    transpiler::{parse_and_check, transpile_to_rust},
};

pub fn build_program(input: &Path, out_dir: Option<&Path>) -> NoerResult<PathBuf> {
    if !input.exists() {
        return fail(format!("source file not found: {}", input.display()));
    }

    let source = fs::read_to_string(input)?;
    let rust_source = transpile_to_rust(&source)?;

    let output_dir = match out_dir {
        Some(path) => path.to_path_buf(),
        None => default_output_dir(input)?,
    };
    fs::create_dir_all(&output_dir)?;

    let stem = input
        .file_stem()
        .and_then(|s| s.to_str())
        .filter(|s| !s.is_empty())
        .unwrap_or("app");

    let rust_file = output_dir.join(format!("{stem}.rs"));
    fs::write(&rust_file, rust_source)?;

    let bin_name = if cfg!(windows) {
        format!("{stem}.exe")
    } else {
        stem.to_string()
    };
    let binary = output_dir.join(bin_name);

    let status = Command::new("rustc")
        .arg("--edition=2021")
        .arg(&rust_file)
        .arg("-o")
        .arg(&binary)
        .status()?;

    if !status.success() {
        return fail(format!("rustc failed for input: {}", input.display()));
    }

    println!("built binary: {}", binary.display());
    Ok(binary)
}

pub fn run_program(input: &Path) -> NoerResult<()> {
    let binary = build_program(input, None)?;

    let status = Command::new(&binary).status()?;
    if !status.success() {
        return fail(format!("program exited with failure: {}", binary.display()));
    }

    Ok(())
}

pub fn check_program(input: &Path) -> NoerResult<()> {
    if !input.exists() {
        return fail(format!("source file not found: {}", input.display()));
    }

    let source = fs::read_to_string(input)?;
    let _ = parse_and_check(&source)?;
    println!("check passed: {}", input.display());
    Ok(())
}

fn default_output_dir(input: &Path) -> NoerResult<PathBuf> {
    if let Some(src_dir) = input.parent() {
        if let Some(project_root) = src_dir.parent() {
            return Ok(project_root.join("target/noer"));
        }
    }

    Ok(env::current_dir()?.join("target/noer"))
}
