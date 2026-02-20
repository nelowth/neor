use std::{fs, path::Path};

use crate::error::{fail, NoerResult};

pub fn format_file(path: &Path, check: bool) -> NoerResult<()> {
    if !path.exists() {
        return fail(format!("source file not found: {}", path.display()));
    }

    let source = fs::read_to_string(path)?;
    let formatted = format_source(&source);

    if check {
        if source != formatted {
            return fail(format!("file is not formatted: {}", path.display()));
        }
        println!("format check passed: {}", path.display());
        return Ok(());
    }

    if source != formatted {
        fs::write(path, formatted)?;
        println!("formatted {}", path.display());
    } else {
        println!("already formatted: {}", path.display());
    }

    Ok(())
}

pub fn format_source(source: &str) -> String {
    let mut out = String::new();
    let mut indent: usize = 0;

    for raw in source.lines() {
        let line = raw.trim();

        if line.is_empty() {
            continue;
        }

        if line == "}" {
            indent = indent.saturating_sub(1);
        }

        out.push_str(&"  ".repeat(indent));

        let normalized = if needs_semicolon(line) {
            format!("{line};")
        } else {
            line.to_string()
        };

        out.push_str(&normalized);
        out.push('\n');

        if line.ends_with('{') {
            indent += 1;
        }
    }

    if out.is_empty() {
        out.push('\n');
    }

    out
}

fn needs_semicolon(line: &str) -> bool {
    let starts_with_statement = line.starts_with("let ")
        || line.starts_with("private let ")
        || line.starts_with("print(")
        || line.starts_with("return")
        || line == "break"
        || line == "continue";

    let assignment_like = line.contains('=')
        && !line.contains("==")
        && !line.contains("!=")
        && !line.contains(">=")
        && !line.contains("<=")
        && !line.starts_with("if ")
        && !line.starts_with("while ")
        && !line.starts_with("let ")
        && !line.starts_with("private let ");

    let expression_stmt_like = line.ends_with(')')
        && !line.starts_with("if ")
        && !line.starts_with("while ")
        && !line.starts_with("print(")
        && !line.starts_with("return")
        && !line.ends_with('{');

    let unary_expr_stmt_like =
        (line.starts_with('&') || line.starts_with('*')) && !line.ends_with('{');

    (starts_with_statement || assignment_like || expression_stmt_like || unary_expr_stmt_like)
        && !line.ends_with(';')
}

#[cfg(test)]
mod tests {
    use super::format_source;

    #[test]
    fn normalizes_indentation_and_semicolons() {
        let input = "main() {\nprint(\"x\")\nlet a = 1\na = a + 1\ncounter.inc()\nCounter::create(1)\n&x\n*d\nbreak\n}\n";
        let output = format_source(input);
        assert_eq!(
            output,
            "main() {\n  print(\"x\");\n  let a = 1;\n  a = a + 1;\n  counter.inc();\n  Counter::create(1);\n  &x;\n  *d;\n  break;\n}\n"
        );
    }
}
