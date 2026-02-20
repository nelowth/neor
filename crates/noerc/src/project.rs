use std::{fs, path::Path};

use crate::{
    error::{fail, NoerResult},
};

pub fn create_new_project(name: &str) -> NoerResult<()> {
    let root = Path::new(name);
    if root.exists() {
        return fail(format!("destination already exists: {}", root.display()));
    }

    fs::create_dir_all(root.join("src"))?;

    let manifest = format!(
        "[package]\nname = \"{}\"\nversion = \"0.1.0\"\nedition = \"2021\"\n",
        sanitize_package_name(name)
    );

    fs::write(root.join("Noer.toml"), manifest)?;
    fs::write(
        root.join("src/main.noer"),
        "main() {\n  print(\"Hello Noer\");\n}\n",
    )?;

    println!("created project at {}", root.display());
    Ok(())
}

fn sanitize_package_name(name: &str) -> String {
    let mut out = String::new();
    for ch in name.chars() {
        if ch.is_ascii_alphanumeric() || ch == '_' || ch == '-' {
            out.push(ch.to_ascii_lowercase());
        } else {
            out.push('-');
        }
    }

    if out.is_empty() {
        "noer-app".to_string()
    } else {
        out
    }
}
