use crate::{
    ast::{
        BinaryOp, Block, Expr, FunctionDecl, ImplDecl, MethodDecl, Program, SelfParamKind, Stmt,
        StructDecl, StructFieldInit, TypeName, UnaryOp,
    },
    error::NoerResult,
    parser, sema,
};

pub fn parse_and_check(source: &str) -> NoerResult<Program> {
    let program = parser::parse(source)?;
    sema::check(&program)?;
    Ok(program)
}

pub fn transpile_to_rust(source: &str) -> NoerResult<String> {
    let program = parse_and_check(source)?;

    let mut out = String::new();

    for decl in &program.structs {
        emit_struct(decl, &mut out);
        out.push('\n');
    }

    for decl in &program.impls {
        emit_impl(decl, &mut out);
        out.push('\n');
    }

    for func in &program.functions {
        emit_function(func, &mut out);
        out.push('\n');
    }

    out.push_str("fn main() {\n");
    emit_block(&program.main, 1, &mut out);
    out.push_str("}\n");

    Ok(out)
}

fn emit_struct(decl: &StructDecl, out: &mut String) {
    out.push_str("#[derive(Clone, Debug, PartialEq)]\n");
    out.push_str(&format!("struct {} {{\n", decl.name));
    for field in &decl.fields {
        out.push_str(&format!("    {}: {},\n", field.name, rust_type(&field.ty)));
    }
    out.push_str("}\n");
}

fn emit_impl(decl: &ImplDecl, out: &mut String) {
    out.push_str(&format!("impl {} {{\n", decl.target));
    for method in &decl.methods {
        emit_method(method, 1, out);
    }
    out.push_str("}\n");
}

fn emit_function(func: &FunctionDecl, out: &mut String) {
    let params = func
        .params
        .iter()
        .map(|p| format!("{}: {}", p.name, rust_type(&p.ty)))
        .collect::<Vec<_>>()
        .join(", ");

    if func.return_type == TypeName::Void {
        out.push_str(&format!("fn {}({}) {{\n", func.name, params));
    } else {
        out.push_str(&format!(
            "fn {}({}) -> {} {{\n",
            func.name,
            params,
            rust_type(&func.return_type)
        ));
    }

    emit_block(&func.body, 1, out);
    out.push_str("}\n");
}

fn emit_method(method: &MethodDecl, indent: usize, out: &mut String) {
    let pad = "    ".repeat(indent);
    let mut params = Vec::new();
    if let Some(receiver) = method.receiver {
        let rendered = match receiver {
            SelfParamKind::Value => "self",
            SelfParamKind::Ref => "&self",
            SelfParamKind::RefMut => "&mut self",
        };
        params.push(rendered.to_string());
    }
    params.extend(
        method
            .params
            .iter()
            .map(|p| format!("{}: {}", p.name, rust_type(&p.ty))),
    );
    let params = params.join(", ");

    if method.return_type == TypeName::Void {
        out.push_str(&format!("{}fn {}({}) {{\n", pad, method.name, params));
    } else {
        out.push_str(&format!(
            "{}fn {}({}) -> {} {{\n",
            pad,
            method.name,
            params,
            rust_type(&method.return_type)
        ));
    }

    emit_block(&method.body, indent + 1, out);
    out.push_str(&format!("{}}}\n", pad));
}

fn rust_type(ty: &TypeName) -> String {
    match ty {
        TypeName::Int => "i64".to_string(),
        TypeName::Float => "f64".to_string(),
        TypeName::Bool => "bool".to_string(),
        TypeName::String => "String".to_string(),
        TypeName::Void => "()".to_string(),
        TypeName::Ref(inner) => format!("&{}", rust_type(inner)),
        TypeName::RefMut(inner) => format!("&mut {}", rust_type(inner)),
        TypeName::Custom(name) => name.clone(),
    }
}

fn emit_block(block: &Block, indent: usize, out: &mut String) {
    for stmt in &block.statements {
        emit_stmt(stmt, indent, out);
    }
}

fn emit_stmt(stmt: &Stmt, indent: usize, out: &mut String) {
    let pad = "    ".repeat(indent);

    match stmt {
        Stmt::Let(let_stmt) => {
            let mutability = if let_stmt.is_mut { "mut " } else { "" };
            let annotation = let_stmt
                .declared_type
                .as_ref()
                .map(|ty| format!(": {}", rust_type(ty)))
                .unwrap_or_default();
            if let Some(value) = &let_stmt.value {
                let expr = transpile_expr(value);
                if let_stmt.is_private {
                    out.push_str(&format!(
                        "{}/* private: encryption hook (future phases) */ let {}{}{} = {};\n",
                        pad, mutability, let_stmt.name, annotation, expr
                    ));
                } else {
                    out.push_str(&format!(
                        "{}let {}{}{} = {};\n",
                        pad, mutability, let_stmt.name, annotation, expr
                    ));
                }
            } else {
                if let_stmt.is_private {
                    out.push_str(&format!(
                        "{}/* private: encryption hook (future phases) */ let {}{}{};\n",
                        pad, mutability, let_stmt.name, annotation
                    ));
                } else {
                    out.push_str(&format!(
                        "{}let {}{}{};\n",
                        pad, mutability, let_stmt.name, annotation
                    ));
                }
            }
        }
        Stmt::Assign(assign_stmt) => {
            let expr = transpile_expr(&assign_stmt.value);
            out.push_str(&format!("{}{} = {};\n", pad, assign_stmt.name, expr));
        }
        Stmt::FieldAssign(assign_stmt) => {
            let expr = transpile_expr(&assign_stmt.value);
            out.push_str(&format!(
                "{}{}.{} = {};\n",
                pad, assign_stmt.object, assign_stmt.field, expr
            ));
        }
        Stmt::Expr(expr) => {
            out.push_str(&format!("{}{};\n", pad, transpile_expr(expr)));
        }
        Stmt::Print(print_stmt) => {
            let expr = transpile_expr(&print_stmt.expr);
            out.push_str(&format!("{}println!(\"{{}}\", {});\n", pad, expr));
        }
        Stmt::If(if_stmt) => {
            let cond = transpile_expr(&if_stmt.condition);
            out.push_str(&format!("{}if {} {{\n", pad, cond));
            emit_block(&if_stmt.then_block, indent + 1, out);
            out.push_str(&format!("{}}}", pad));

            if let Some(else_block) = &if_stmt.else_block {
                out.push_str(" else {\n");
                emit_block(else_block, indent + 1, out);
                out.push_str(&format!("{}}}\n", pad));
            } else {
                out.push('\n');
            }
        }
        Stmt::While(while_stmt) => {
            let cond = transpile_expr(&while_stmt.condition);
            out.push_str(&format!("{}while {} {{\n", pad, cond));
            emit_block(&while_stmt.body, indent + 1, out);
            out.push_str(&format!("{}}}\n", pad));
        }
        Stmt::Loop(loop_stmt) => {
            out.push_str(&format!("{}loop {{\n", pad));
            emit_block(&loop_stmt.body, indent + 1, out);
            out.push_str(&format!("{}}}\n", pad));
        }
        Stmt::Break => {
            out.push_str(&format!("{}break;\n", pad));
        }
        Stmt::Continue => {
            out.push_str(&format!("{}continue;\n", pad));
        }
        Stmt::Return(None) => {
            out.push_str(&format!("{}return;\n", pad));
        }
        Stmt::Return(Some(expr)) => {
            out.push_str(&format!("{}return {};\n", pad, transpile_expr(expr)));
        }
    }
}

fn transpile_expr(expr: &Expr) -> String {
    transpile_expr_with_prec(expr, 0)
}

fn transpile_expr_with_prec(expr: &Expr, parent_prec: u8) -> String {
    match expr {
        Expr::Int(v) => v.to_string(),
        Expr::Float(v) => {
            let mut s = v.to_string();
            if !s.contains('.') && !s.contains('e') && !s.contains('E') {
                s.push_str(".0");
            }
            s
        }
        Expr::Bool(v) => v.to_string(),
        Expr::String(v) => format!("String::from({:?})", v),
        Expr::Ident(name) => name.clone(),
        Expr::StructInit { name, fields } => {
            let fields = transpile_struct_fields(fields);
            format!("{} {{ {} }}", name, fields)
        }
        Expr::Call { callee, args } => {
            let args = args
                .iter()
                .map(transpile_expr)
                .collect::<Vec<_>>()
                .join(", ");
            format!("{}({})", callee, args)
        }
        Expr::StaticCall {
            target,
            method,
            args,
        } => {
            let args = args
                .iter()
                .map(transpile_expr)
                .collect::<Vec<_>>()
                .join(", ");
            format!("{}::{}({})", target, method, args)
        }
        Expr::MethodCall {
            object,
            method,
            args,
        } => {
            let object = transpile_expr_with_prec(object, 7);
            let args = args
                .iter()
                .map(transpile_expr)
                .collect::<Vec<_>>()
                .join(", ");
            format!("{}.{}({})", object, method, args)
        }
        Expr::FieldAccess { object, field } => {
            let object = transpile_expr_with_prec(object, 7);
            format!("{}.{}", object, field)
        }
        Expr::Unary { op, expr } => {
            let rendered = match op {
                UnaryOp::Neg => format!("-{}", transpile_expr_with_prec(expr, 7)),
                UnaryOp::Not => format!("!{}", transpile_expr_with_prec(expr, 7)),
                UnaryOp::Ref => format!("&{}", transpile_expr_with_prec(expr, 7)),
                UnaryOp::RefMut => format!("&mut {}", transpile_expr_with_prec(expr, 7)),
                UnaryOp::Deref => format!("*{}", transpile_expr_with_prec(expr, 7)),
            };
            if 7 < parent_prec {
                format!("({})", rendered)
            } else {
                rendered
            }
        }
        Expr::Binary { left, op, right } => {
            let op_str = match op {
                BinaryOp::Or => "||",
                BinaryOp::And => "&&",
                BinaryOp::Eq => "==",
                BinaryOp::Ne => "!=",
                BinaryOp::Lt => "<",
                BinaryOp::Le => "<=",
                BinaryOp::Gt => ">",
                BinaryOp::Ge => ">=",
                BinaryOp::Add => "+",
                BinaryOp::Sub => "-",
                BinaryOp::Mul => "*",
                BinaryOp::Div => "/",
            };
            let my_prec = op_precedence(*op);
            let left = transpile_expr_with_prec(left, my_prec);
            let right = transpile_expr_with_prec(right, my_prec + 1);
            let rendered = format!("{} {} {}", left, op_str, right);
            if my_prec < parent_prec {
                format!("({})", rendered)
            } else {
                rendered
            }
        }
        Expr::Group(inner) => format!("({})", transpile_expr_with_prec(inner, 0)),
    }
}

fn transpile_struct_fields(fields: &[StructFieldInit]) -> String {
    fields
        .iter()
        .map(|f| format!("{}: {}", f.name, transpile_expr(&f.value)))
        .collect::<Vec<_>>()
        .join(", ")
}

fn op_precedence(op: BinaryOp) -> u8 {
    match op {
        BinaryOp::Or => 1,
        BinaryOp::And => 2,
        BinaryOp::Eq | BinaryOp::Ne => 3,
        BinaryOp::Lt | BinaryOp::Le | BinaryOp::Gt | BinaryOp::Ge => 4,
        BinaryOp::Add | BinaryOp::Sub => 5,
        BinaryOp::Mul | BinaryOp::Div => 6,
    }
}

#[cfg(test)]
mod tests {
    use super::transpile_to_rust;

    #[test]
    fn transpiles_struct_and_field_access() {
        let source = "struct User { id: int, name: string, } main() { let u = User { id: 1, name: \"Ana\" }; let id = u.id; print(id); }";
        let rust = transpile_to_rust(source).expect("transpile failed");
        assert!(rust.contains("struct User {"));
        assert!(rust.contains("id: i64,"));
        assert!(rust.contains("name: String,"));
        assert!(rust.contains("let u = User { id: 1, name: String::from(\"Ana\") };"));
        assert!(rust.contains("let id = u.id;"));
    }

    #[test]
    fn transpiles_impl_method_and_method_call() {
        let source = "struct User { score: int, } impl User { fn score_value(&self) -> int { return self.score; } } main() { let u = User { score: 7 }; let x = u.score_value(); print(x); }";
        let rust = transpile_to_rust(source).expect("transpile failed");
        assert!(rust.contains("impl User {"));
        assert!(rust.contains("fn score_value(&self) -> i64 {"));
        assert!(rust.contains("return self.score;"));
        assert!(rust.contains("let x = u.score_value();"));
    }

    #[test]
    fn transpiles_field_assignment() {
        let source = "struct User { score: int, } main() { let mut u = User { score: 1 }; u.score = u.score + 1; print(u.score); }";
        let rust = transpile_to_rust(source).expect("transpile failed");
        assert!(rust.contains("u.score = u.score + 1;"));
    }

    #[test]
    fn transpiles_static_method_call() {
        let source = "struct User { name: string, } impl User { fn make(name: string) -> User { return User { name: name }; } } main() { let u = User::make(\"Ana\"); print(u.name); }";
        let rust = transpile_to_rust(source).expect("transpile failed");
        assert!(rust.contains("fn make(name: String) -> User {"));
        assert!(rust.contains("let u = User::make(String::from(\"Ana\"));"));
    }

    #[test]
    fn transpiles_typed_let_and_expr_statement() {
        let source = "struct Counter { value: int, } impl Counter { fn inc(&mut self) { self.value = self.value + 1; } } main() { let mut c: Counter = Counter { value: 0 }; c.inc(); let value: int = c.value; print(value); }";
        let rust = transpile_to_rust(source).expect("transpile failed");
        assert!(rust.contains("let mut c: Counter = Counter { value: 0 };"));
        assert!(rust.contains("c.inc();"));
        assert!(rust.contains("let value: i64 = c.value;"));
    }

    #[test]
    fn transpiles_typed_let_without_initializer() {
        let source = "main() { let mut x: int; x = 1; print(x); }";
        let rust = transpile_to_rust(source).expect("transpile failed");
        assert!(rust.contains("let mut x: i64;"));
        assert!(rust.contains("x = 1;"));
    }

    #[test]
    fn transpiles_expression_precedence() {
        let source = "main() { let x = 1 + 2 * 3; print(x); }";
        let rust = transpile_to_rust(source).expect("transpile failed");
        assert!(rust.contains("let x = 1 + 2 * 3;"));
    }

    #[test]
    fn transpiles_function_and_call() {
        let source = "fn sum(a: int, b: int) -> int { return a + b; } main() { let x = sum(1, 2); print(x); }";
        let rust = transpile_to_rust(source).expect("transpile failed");
        assert!(rust.contains("fn sum(a: i64, b: i64) -> i64 {"));
        assert!(rust.contains("return a + b;"));
        assert!(rust.contains("let x = sum(1, 2);"));
    }

    #[test]
    fn transpiles_if_else() {
        let source = "main() { let x = 1; if (x >= 1) { print(1); } else { print(0); } }";
        let rust = transpile_to_rust(source).expect("transpile failed");
        assert!(rust.contains("if x >= 1 {"));
        assert!(rust.contains("} else {"));
    }

    #[test]
    fn transpiles_while_loop_assignment_and_control_flow() {
        let source = "main() { let mut i = 0; while (i < 3) { i = i + 1; } loop { break; } }";
        let rust = transpile_to_rust(source).expect("transpile failed");
        assert!(rust.contains("while i < 3 {"));
        assert!(rust.contains("i = i + 1;"));
        assert!(rust.contains("loop {"));
        assert!(rust.contains("break;"));
    }

    #[test]
    fn transpiles_ref_types_and_borrow_deref_unary() {
        let source = "fn read(v: &int) -> int { return *v; } fn touch(v: &mut int) -> int { return *v; } main() { let mut x = 1; let a = read(&x); let b = touch(&mut x); print(a); print(b); }";
        let rust = transpile_to_rust(source).expect("transpile failed");
        assert!(rust.contains("fn read(v: &i64) -> i64"));
        assert!(rust.contains("fn touch(v: &mut i64) -> i64"));
        assert!(rust.contains("return *v;"));
        assert!(rust.contains("let a = read(&x);"));
        assert!(rust.contains("let b = touch(&mut x);"));
    }

    #[test]
    fn transpiles_private_let_with_hook_comment() {
        let source = "main() { private let token = \"abc\"; print(1); }";
        let rust = transpile_to_rust(source).expect("transpile failed");
        assert!(rust.contains("private: encryption hook"));
        assert!(rust.contains("let token = String::from(\"abc\");"));
    }

    #[test]
    fn rejects_missing_main_block() {
        let source = "print(1);";
        assert!(transpile_to_rust(source).is_err());
    }
}
