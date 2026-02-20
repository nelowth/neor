use std::collections::{HashMap, HashSet};

use crate::{
    ast::{
        BinaryOp, Block, Expr, FunctionDecl, MethodDecl, Program, SelfParamKind, Stmt, TypeName,
        UnaryOp,
    },
    error::{fail, NoerResult},
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Type {
    Int,
    Float,
    Bool,
    String,
    Void,
    Ref { inner: usize, mutable: bool },
    Struct(String),
}

#[derive(Debug, Clone, Copy)]
struct Symbol {
    id: usize,
    ty: usize,
    is_mut: bool,
    is_initialized: bool,
    is_moved: bool,
    is_private: bool,
}

#[derive(Debug, Clone, Copy, Default)]
struct ActiveBorrow {
    shared_count: usize,
    mutable: bool,
}

#[derive(Debug, Clone)]
struct FunctionSig {
    params: Vec<usize>,
    return_type: usize,
}

#[derive(Debug, Clone)]
struct MethodSig {
    receiver: Option<SelfParamKind>,
    params: Vec<usize>,
    return_type: usize,
}

#[derive(Debug, Clone)]
struct StructDef {
    fields: HashMap<String, usize>,
    methods: HashMap<String, MethodSig>,
}

#[derive(Debug, Clone)]
struct TypeTable {
    types: Vec<Type>,
}

impl TypeTable {
    fn new() -> Self {
        Self {
            types: vec![Type::Int, Type::Float, Type::Bool, Type::String, Type::Void],
        }
    }

    fn intern(&mut self, ty: Type) -> usize {
        if let Some((idx, _)) = self.types.iter().enumerate().find(|(_, t)| **t == ty) {
            idx
        } else {
            self.types.push(ty);
            self.types.len() - 1
        }
    }

    fn get(&self, id: usize) -> &Type {
        &self.types[id]
    }

    fn name(&self, id: usize) -> String {
        match self.get(id) {
            Type::Int => "int".to_string(),
            Type::Float => "float".to_string(),
            Type::Bool => "bool".to_string(),
            Type::String => "string".to_string(),
            Type::Void => "void".to_string(),
            Type::Ref { inner, mutable } => {
                if *mutable {
                    format!("&mut {}", self.name(*inner))
                } else {
                    format!("&{}", self.name(*inner))
                }
            }
            Type::Struct(name) => name.clone(),
        }
    }

    fn is_numeric(&self, id: usize) -> bool {
        matches!(self.get(id), Type::Int | Type::Float)
    }

    fn is_reference(&self, id: usize) -> bool {
        matches!(self.get(id), Type::Ref { .. })
    }

    fn is_copy_type(&self, id: usize) -> bool {
        matches!(
            self.get(id),
            Type::Int | Type::Float | Type::Bool | Type::Ref { .. }
        )
    }
}

#[derive(Debug, Clone)]
struct Env {
    scopes: Vec<HashMap<String, Symbol>>,
    ref_bindings_scopes: Vec<HashMap<usize, (usize, bool)>>,
    active_borrows: HashMap<usize, ActiveBorrow>,
    next_symbol_id: usize,
    loop_depth: usize,
    functions: HashMap<String, FunctionSig>,
    structs: HashMap<String, StructDef>,
    current_return: Option<usize>,
    saw_return: bool,
}

impl Env {
    fn new(
        functions: HashMap<String, FunctionSig>,
        structs: HashMap<String, StructDef>,
        current_return: Option<usize>,
    ) -> Self {
        Self {
            scopes: vec![HashMap::new()],
            ref_bindings_scopes: vec![HashMap::new()],
            active_borrows: HashMap::new(),
            next_symbol_id: 0,
            loop_depth: 0,
            functions,
            structs,
            current_return,
            saw_return: false,
        }
    }

    fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
        self.ref_bindings_scopes.push(HashMap::new());
    }

    fn exit_scope(&mut self) {
        self.scopes
            .pop()
            .expect("scope stack should never be empty");
        let bindings = self
            .ref_bindings_scopes
            .pop()
            .expect("ref binding stack should never be empty");
        for (_, (source_id, is_mut)) in bindings {
            self.release_active_borrow(source_id, is_mut);
        }
    }

    fn define(&mut self, name: String, mut symbol: Symbol) -> NoerResult<()> {
        let current = self
            .scopes
            .last_mut()
            .expect("scope stack should never be empty");

        if current.contains_key(&name) {
            return fail(format!("variable `{}` already defined in this scope", name));
        }

        symbol.id = self.next_symbol_id;
        self.next_symbol_id += 1;
        current.insert(name, symbol);
        Ok(())
    }

    fn lookup(&self, name: &str) -> Option<&Symbol> {
        for scope in self.scopes.iter().rev() {
            if let Some(sym) = scope.get(name) {
                return Some(sym);
            }
        }
        None
    }

    fn lookup_mut(&mut self, name: &str) -> Option<&mut Symbol> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(sym) = scope.get_mut(name) {
                return Some(sym);
            }
        }
        None
    }

    fn activate_lexical_borrow(
        &mut self,
        source_id: usize,
        source_name: &str,
        is_mut: bool,
    ) -> NoerResult<()> {
        let entry = self.active_borrows.entry(source_id).or_default();
        if is_mut {
            if entry.mutable || entry.shared_count > 0 {
                return fail(format!(
                    "cannot take mutable borrow of `{}` because it is already borrowed",
                    source_name
                ));
            }
            entry.mutable = true;
        } else {
            if entry.mutable {
                return fail(format!(
                    "cannot take shared borrow of `{}` because it is mutably borrowed",
                    source_name
                ));
            }
            entry.shared_count += 1;
        }
        Ok(())
    }

    fn release_active_borrow(&mut self, source_id: usize, is_mut: bool) {
        let should_remove = if let Some(entry) = self.active_borrows.get_mut(&source_id) {
            if is_mut {
                entry.mutable = false;
            } else if entry.shared_count > 0 {
                entry.shared_count -= 1;
            }
            !entry.mutable && entry.shared_count == 0
        } else {
            false
        };

        if should_remove {
            self.active_borrows.remove(&source_id);
        }
    }

    fn has_active_mut_borrow(&self, symbol_id: usize) -> bool {
        self.active_borrows
            .get(&symbol_id)
            .map(|b| b.mutable)
            .unwrap_or(false)
    }

    fn has_any_active_borrow(&self, symbol_id: usize) -> bool {
        self.active_borrows
            .get(&symbol_id)
            .map(|b| b.mutable || b.shared_count > 0)
            .unwrap_or(false)
    }

    fn register_ref_binding(
        &mut self,
        var_id: usize,
        source_id: usize,
        source_name: &str,
        is_mut: bool,
    ) -> NoerResult<()> {
        self.activate_lexical_borrow(source_id, source_name, is_mut)?;
        self.ref_bindings_scopes
            .last_mut()
            .expect("ref binding stack should never be empty")
            .insert(var_id, (source_id, is_mut));
        Ok(())
    }

    fn rebind_ref_binding(
        &mut self,
        var_id: usize,
        source_id: usize,
        source_name: &str,
        is_mut: bool,
    ) -> NoerResult<()> {
        let mut found_scope = None;
        let mut old_binding = None;
        for idx in (0..self.ref_bindings_scopes.len()).rev() {
            if let Some(binding) = self.ref_bindings_scopes[idx].get(&var_id).copied() {
                found_scope = Some(idx);
                old_binding = Some(binding);
                break;
            }
        }

        let Some(scope_idx) = found_scope else {
            return fail("internal error: missing reference binding metadata");
        };
        let Some((old_source_id, old_mut)) = old_binding else {
            return fail("internal error: missing reference binding metadata");
        };

        self.release_active_borrow(old_source_id, old_mut);
        if let Err(err) = self.activate_lexical_borrow(source_id, source_name, is_mut) {
            let _ = self.activate_lexical_borrow(old_source_id, source_name, old_mut);
            return Err(err);
        }
        self.ref_bindings_scopes[scope_idx].insert(var_id, (source_id, is_mut));
        Ok(())
    }
}

pub fn check(program: &Program) -> NoerResult<()> {
    let mut types = TypeTable::new();
    let (mut structs, struct_names) = collect_struct_defs(program, &mut types)?;
    collect_method_signatures(program, &mut structs, &mut types, &struct_names)?;
    let functions = collect_function_signatures(program, &mut types, &struct_names)?;
    preintern_reference_types(&mut types);

    for func in &program.functions {
        check_function(func, &functions, &structs, &mut types, &struct_names)?;
    }

    for impl_decl in &program.impls {
        let Some(self_ty) = find_struct_type_id(&types, &impl_decl.target) else {
            return fail(format!(
                "internal error: missing struct type `{}`",
                impl_decl.target
            ));
        };

        for method in &impl_decl.methods {
            let Some(def) = structs.get(&impl_decl.target) else {
                return fail(format!(
                    "internal error: missing struct def `{}`",
                    impl_decl.target
                ));
            };
            let Some(sig) = def.methods.get(&method.name).cloned() else {
                return fail(format!(
                    "internal error: missing method `{}` on `{}`",
                    method.name, impl_decl.target
                ));
            };

            check_method(
                method,
                self_ty,
                &functions,
                &structs,
                &types,
                sig.receiver,
                sig.return_type,
                &sig.params,
            )?;
        }
    }

    let mut main_env = Env::new(functions, structs, None);
    check_block(&program.main, &mut main_env, &types)
}

fn preintern_reference_types(types: &mut TypeTable) {
    let ids: Vec<usize> = (0..types.types.len()).collect();
    for id in ids {
        if matches!(types.get(id), Type::Void | Type::Ref { .. }) {
            continue;
        }
        let _ = types.intern(Type::Ref {
            inner: id,
            mutable: false,
        });
        let _ = types.intern(Type::Ref {
            inner: id,
            mutable: true,
        });
    }
}

fn collect_struct_defs(
    program: &Program,
    types: &mut TypeTable,
) -> NoerResult<(HashMap<String, StructDef>, HashSet<String>)> {
    let mut names = HashSet::new();
    for decl in &program.structs {
        if !names.insert(decl.name.clone()) {
            return fail(format!("struct `{}` already defined", decl.name));
        }
    }

    for name in &names {
        let _ = types.intern(Type::Struct(name.clone()));
    }

    let mut defs = HashMap::new();
    for decl in &program.structs {
        let mut fields = HashMap::new();
        for field in &decl.fields {
            if fields.contains_key(&field.name) {
                return fail(format!(
                    "field `{}` duplicated in struct `{}`",
                    field.name, decl.name
                ));
            }
            let ty = resolve_type_name(field.ty.clone(), types, &names)?;
            if matches!(types.get(ty), Type::Void) {
                return fail(format!(
                    "field `{}` in struct `{}` cannot be void",
                    field.name, decl.name
                ));
            }
            if types.is_reference(ty) {
                return fail(format!(
                    "field `{}` in struct `{}` cannot be a reference in this MVP phase",
                    field.name, decl.name
                ));
            }
            fields.insert(field.name.clone(), ty);
        }
        defs.insert(
            decl.name.clone(),
            StructDef {
                fields,
                methods: HashMap::new(),
            },
        );
    }

    Ok((defs, names))
}

fn collect_method_signatures(
    program: &Program,
    structs: &mut HashMap<String, StructDef>,
    types: &mut TypeTable,
    struct_names: &HashSet<String>,
) -> NoerResult<()> {
    for impl_decl in &program.impls {
        let Some(def) = structs.get_mut(&impl_decl.target) else {
            return fail(format!("unknown struct `{}` in `impl`", impl_decl.target));
        };

        for method in &impl_decl.methods {
            if def.methods.contains_key(&method.name) {
                return fail(format!(
                    "method `{}` already defined for struct `{}`",
                    method.name, impl_decl.target
                ));
            }

            let mut params = Vec::with_capacity(method.params.len());
            for param in &method.params {
                let param_ty = resolve_type_name(param.ty.clone(), types, struct_names)?;
                if matches!(types.get(param_ty), Type::Void) {
                    return fail(format!(
                        "method `{}` cannot have `void` parameter `{}`",
                        method.name, param.name
                    ));
                }
                params.push(param_ty);
            }

            let return_type = resolve_type_name(method.return_type.clone(), types, struct_names)?;
            if types.is_reference(return_type) {
                return fail(format!(
                    "method `{}` cannot return references in this MVP phase",
                    method.name
                ));
            }

            def.methods.insert(
                method.name.clone(),
                MethodSig {
                    receiver: method.receiver,
                    params,
                    return_type,
                },
            );
        }
    }

    Ok(())
}

fn collect_function_signatures(
    program: &Program,
    types: &mut TypeTable,
    struct_names: &HashSet<String>,
) -> NoerResult<HashMap<String, FunctionSig>> {
    let mut functions = HashMap::new();

    for func in &program.functions {
        if functions.contains_key(&func.name) {
            return fail(format!("function `{}` already defined", func.name));
        }

        let mut params = Vec::with_capacity(func.params.len());
        for p in &func.params {
            params.push(resolve_type_name(p.ty.clone(), types, struct_names)?);
        }
        let return_type = resolve_type_name(func.return_type.clone(), types, struct_names)?;
        if types.is_reference(return_type) {
            return fail(format!(
                "function `{}` cannot return references in this MVP phase",
                func.name
            ));
        }

        functions.insert(
            func.name.clone(),
            FunctionSig {
                params,
                return_type,
            },
        );
    }

    Ok(functions)
}

fn check_function(
    func: &FunctionDecl,
    functions: &HashMap<String, FunctionSig>,
    structs: &HashMap<String, StructDef>,
    types: &mut TypeTable,
    struct_names: &HashSet<String>,
) -> NoerResult<()> {
    let return_ty = resolve_type_name(func.return_type.clone(), types, struct_names)?;
    let mut env = Env::new(functions.clone(), structs.clone(), Some(return_ty));

    for param in &func.params {
        let param_ty = resolve_type_name(param.ty.clone(), types, struct_names)?;
        if matches!(types.get(param_ty), Type::Void) {
            return fail(format!(
                "function `{}` cannot have `void` parameter `{}`",
                func.name, param.name
            ));
        }
        env.define(
            param.name.clone(),
            Symbol {
                id: 0,
                ty: param_ty,
                is_mut: false,
                is_initialized: true,
                is_moved: false,
                is_private: false,
            },
        )?;
    }

    check_block(&func.body, &mut env, types)?;

    if !matches!(types.get(return_ty), Type::Void) && !block_guarantees_return(&func.body) {
        return fail(format!(
            "function `{}` must return `{}`",
            func.name,
            types.name(return_ty)
        ));
    }

    Ok(())
}

fn check_method(
    method: &MethodDecl,
    self_ty: usize,
    functions: &HashMap<String, FunctionSig>,
    structs: &HashMap<String, StructDef>,
    types: &TypeTable,
    receiver: Option<SelfParamKind>,
    return_ty: usize,
    params: &[usize],
) -> NoerResult<()> {
    if method.receiver != receiver {
        return fail("internal error: method receiver mismatch");
    }

    let mut env = Env::new(functions.clone(), structs.clone(), Some(return_ty));

    if let Some(receiver) = receiver {
        env.define(
            "self".to_string(),
            Symbol {
                id: 0,
                ty: self_ty,
                is_mut: matches!(receiver, SelfParamKind::RefMut),
                is_initialized: true,
                is_moved: false,
                is_private: false,
            },
        )?;
    }

    for (param, ty) in method.params.iter().zip(params) {
        env.define(
            param.name.clone(),
            Symbol {
                id: 0,
                ty: *ty,
                is_mut: false,
                is_initialized: true,
                is_moved: false,
                is_private: false,
            },
        )?;
    }

    check_block(&method.body, &mut env, types)?;

    if !matches!(types.get(return_ty), Type::Void) && !block_guarantees_return(&method.body) {
        return fail(format!(
            "method `{}` must return `{}`",
            method.name,
            types.name(return_ty)
        ));
    }

    Ok(())
}

fn resolve_type_name(
    ty: TypeName,
    types: &mut TypeTable,
    struct_names: &HashSet<String>,
) -> NoerResult<usize> {
    let resolved = match ty {
        TypeName::Int => Type::Int,
        TypeName::Float => Type::Float,
        TypeName::Bool => Type::Bool,
        TypeName::String => Type::String,
        TypeName::Void => Type::Void,
        TypeName::Ref(inner) => {
            let inner_ty = resolve_type_name(*inner, types, struct_names)?;
            if matches!(types.get(inner_ty), Type::Void) {
                return fail("reference to `void` is not allowed");
            }
            Type::Ref {
                inner: inner_ty,
                mutable: false,
            }
        }
        TypeName::RefMut(inner) => {
            let inner_ty = resolve_type_name(*inner, types, struct_names)?;
            if matches!(types.get(inner_ty), Type::Void) {
                return fail("reference to `void` is not allowed");
            }
            Type::Ref {
                inner: inner_ty,
                mutable: true,
            }
        }
        TypeName::Custom(name) => {
            if !struct_names.contains(&name) {
                return fail(format!("unknown type `{}`", name));
            }
            Type::Struct(name)
        }
    };

    Ok(types.intern(resolved))
}

fn find_struct_type_id(types: &TypeTable, name: &str) -> Option<usize> {
    types
        .types
        .iter()
        .enumerate()
        .find(|(_, t)| matches!(t, Type::Struct(n) if n == name))
        .map(|(id, _)| id)
}

fn find_ref_type_id(types: &TypeTable, inner: usize, mutable: bool) -> Option<usize> {
    types
        .types
        .iter()
        .enumerate()
        .find(
            |(_, t)| matches!(t, Type::Ref { inner: i, mutable: m } if *i == inner && *m == mutable),
        )
        .map(|(id, _)| id)
}

fn resolve_local_type_name(ty: &TypeName, env: &Env, types: &TypeTable) -> NoerResult<usize> {
    match ty {
        TypeName::Int => Ok(0),
        TypeName::Float => Ok(1),
        TypeName::Bool => Ok(2),
        TypeName::String => Ok(3),
        TypeName::Void => Ok(4),
        TypeName::Ref(inner) => {
            let inner_ty = resolve_local_type_name(inner, env, types)?;
            if matches!(types.get(inner_ty), Type::Void) {
                return fail("reference to `void` is not allowed");
            }
            if let Some(id) = find_ref_type_id(types, inner_ty, false) {
                Ok(id)
            } else {
                fail(format!(
                    "internal error: missing reference type `&{}`",
                    types.name(inner_ty)
                ))
            }
        }
        TypeName::RefMut(inner) => {
            let inner_ty = resolve_local_type_name(inner, env, types)?;
            if matches!(types.get(inner_ty), Type::Void) {
                return fail("reference to `void` is not allowed");
            }
            if let Some(id) = find_ref_type_id(types, inner_ty, true) {
                Ok(id)
            } else {
                fail(format!(
                    "internal error: missing reference type `&mut {}`",
                    types.name(inner_ty)
                ))
            }
        }
        TypeName::Custom(name) => {
            if !env.structs.contains_key(name) {
                return fail(format!("unknown type `{}`", name));
            }
            if let Some(id) = find_struct_type_id(types, name) {
                Ok(id)
            } else {
                fail(format!("internal error: missing struct type `{}`", name))
            }
        }
    }
}

fn check_block(block: &Block, env: &mut Env, types: &TypeTable) -> NoerResult<()> {
    env.enter_scope();

    for stmt in &block.statements {
        check_stmt(stmt, env, types)?;
        let stmt_flow = analyze_stmt_flow(stmt, env.loop_depth > 0);
        if !stmt_flow.can_complete_normally {
            break;
        }
    }

    env.exit_scope();
    Ok(())
}

fn merge_definite_initialization(base_env: &mut Env, then_env: &Env, else_env: &Env) {
    for (scope_idx, base_scope) in base_env.scopes.iter_mut().enumerate() {
        let Some(then_scope) = then_env.scopes.get(scope_idx) else {
            continue;
        };
        let Some(else_scope) = else_env.scopes.get(scope_idx) else {
            continue;
        };

        for (name, base_symbol) in base_scope.iter_mut() {
            let Some(then_symbol) = then_scope.get(name) else {
                continue;
            };
            let Some(else_symbol) = else_scope.get(name) else {
                continue;
            };

            base_symbol.is_initialized = then_symbol.is_initialized && else_symbol.is_initialized;
            base_symbol.is_moved =
                base_symbol.is_initialized && (then_symbol.is_moved || else_symbol.is_moved);
        }
    }
}

fn copy_definite_initialization(base_env: &mut Env, from_env: &Env) {
    for (scope_idx, base_scope) in base_env.scopes.iter_mut().enumerate() {
        let Some(from_scope) = from_env.scopes.get(scope_idx) else {
            continue;
        };

        for (name, base_symbol) in base_scope.iter_mut() {
            let Some(from_symbol) = from_scope.get(name) else {
                continue;
            };
            base_symbol.is_initialized = from_symbol.is_initialized;
            base_symbol.is_moved = from_symbol.is_moved;
        }
    }
}

fn merge_possible_moved_from_loop(base_env: &mut Env, loop_env: &Env) {
    for (scope_idx, base_scope) in base_env.scopes.iter_mut().enumerate() {
        let Some(loop_scope) = loop_env.scopes.get(scope_idx) else {
            continue;
        };

        for (name, base_symbol) in base_scope.iter_mut() {
            let Some(loop_symbol) = loop_scope.get(name) else {
                continue;
            };
            base_symbol.is_moved = base_symbol.is_moved || loop_symbol.is_moved;
        }
    }
}

fn block_guarantees_return(block: &Block) -> bool {
    let flow = analyze_block_flow(block, false);
    !flow.can_complete_normally && flow.can_return
}

#[derive(Debug, Clone, Copy, Default)]
struct FlowFacts {
    can_complete_normally: bool,
    can_return: bool,
    can_break: bool,
    can_continue: bool,
}

#[derive(Debug, Default)]
struct BorrowContext {
    shared: HashSet<usize>,
    mutable: HashSet<usize>,
}

impl BorrowContext {
    fn borrow_shared(&mut self, symbol_id: usize, display_name: &str) -> NoerResult<()> {
        if self.mutable.contains(&symbol_id) {
            return fail(format!(
                "cannot take shared borrow of `{}` because it is already mutably borrowed in this expression",
                display_name
            ));
        }
        self.shared.insert(symbol_id);
        Ok(())
    }

    fn borrow_mut(&mut self, symbol_id: usize, display_name: &str) -> NoerResult<()> {
        if self.mutable.contains(&symbol_id) || self.shared.contains(&symbol_id) {
            return fail(format!(
                "cannot take mutable borrow of `{}` because it is already borrowed in this expression",
                display_name
            ));
        }
        self.mutable.insert(symbol_id);
        Ok(())
    }

    fn is_borrowed(&self, symbol_id: usize) -> bool {
        self.mutable.contains(&symbol_id) || self.shared.contains(&symbol_id)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ValueUse {
    ByValue,
    ReadOnly,
}

fn analyze_block_flow(block: &Block, in_loop: bool) -> FlowFacts {
    let mut combined = FlowFacts {
        can_complete_normally: true,
        ..FlowFacts::default()
    };

    for stmt in &block.statements {
        if !combined.can_complete_normally {
            break;
        }

        let stmt_flow = analyze_stmt_flow(stmt, in_loop);
        combined.can_return |= stmt_flow.can_return;
        combined.can_break |= stmt_flow.can_break;
        combined.can_continue |= stmt_flow.can_continue;
        combined.can_complete_normally = stmt_flow.can_complete_normally;
    }

    combined
}

fn analyze_stmt_flow(stmt: &Stmt, in_loop: bool) -> FlowFacts {
    match stmt {
        Stmt::Let(_) | Stmt::Assign(_) | Stmt::FieldAssign(_) | Stmt::Expr(_) | Stmt::Print(_) => {
            FlowFacts {
                can_complete_normally: true,
                ..FlowFacts::default()
            }
        }
        Stmt::Return(_) => FlowFacts {
            can_return: true,
            ..FlowFacts::default()
        },
        Stmt::Break => FlowFacts {
            can_break: in_loop,
            ..FlowFacts::default()
        },
        Stmt::Continue => FlowFacts {
            can_continue: in_loop,
            ..FlowFacts::default()
        },
        Stmt::If(if_stmt) => {
            let then_flow = analyze_block_flow(&if_stmt.then_block, in_loop);
            let else_flow = if let Some(else_block) = &if_stmt.else_block {
                analyze_block_flow(else_block, in_loop)
            } else {
                FlowFacts {
                    can_complete_normally: true,
                    ..FlowFacts::default()
                }
            };

            FlowFacts {
                can_complete_normally: then_flow.can_complete_normally
                    || else_flow.can_complete_normally,
                can_return: then_flow.can_return || else_flow.can_return,
                can_break: then_flow.can_break || else_flow.can_break,
                can_continue: then_flow.can_continue || else_flow.can_continue,
            }
        }
        Stmt::While(while_stmt) => {
            let body_flow = analyze_block_flow(&while_stmt.body, true);
            FlowFacts {
                // `while` may execute zero times.
                can_complete_normally: true,
                can_return: body_flow.can_return,
                can_break: false,
                can_continue: false,
            }
        }
        Stmt::Loop(loop_stmt) => {
            let body_flow = analyze_block_flow(&loop_stmt.body, true);
            FlowFacts {
                // `loop` only exits when some path reaches `break`.
                can_complete_normally: body_flow.can_break,
                can_return: body_flow.can_return,
                can_break: false,
                can_continue: false,
            }
        }
    }
}

fn check_stmt(stmt: &Stmt, env: &mut Env, types: &TypeTable) -> NoerResult<()> {
    match stmt {
        Stmt::Let(let_stmt) => {
            let declared_ty = if let Some(expected_name) = &let_stmt.declared_type {
                Some(resolve_local_type_name(expected_name, env, types)?)
            } else {
                None
            };

            let (ty, is_initialized) = match &let_stmt.value {
                Some(value) => {
                    let mut borrows = BorrowContext::default();
                    let value_ty = infer_expr_type_with_borrows(
                        value,
                        env,
                        types,
                        &mut borrows,
                        ValueUse::ByValue,
                    )?;
                    let value_is_private = expr_is_private(value, env)?;
                    if matches!(types.get(value_ty), Type::Void) {
                        return fail(format!(
                            "cannot assign `void` value to variable `{}`",
                            let_stmt.name
                        ));
                    }
                    if value_is_private && !let_stmt.is_private {
                        return fail(format!(
                            "cannot store private value in non-private variable `{}`",
                            let_stmt.name
                        ));
                    }

                    if let Some(expected_ty) = declared_ty {
                        if value_ty != expected_ty {
                            return fail(format!(
                                "type mismatch in declaration of `{}`: expected `{}`, found `{}`",
                                let_stmt.name,
                                types.name(expected_ty),
                                types.name(value_ty)
                            ));
                        }
                        (expected_ty, true)
                    } else {
                        (value_ty, true)
                    }
                }
                None => {
                    let Some(expected_ty) = declared_ty else {
                        return fail("`let` without initializer requires explicit type");
                    };
                    if matches!(types.get(expected_ty), Type::Void) {
                        return fail(format!(
                            "cannot declare variable `{}` with type `void`",
                            let_stmt.name
                        ));
                    }
                    if types.is_reference(expected_ty) {
                        return fail("reference variables require initializer in this MVP phase");
                    }
                    (expected_ty, false)
                }
            };

            let borrow_binding = let_stmt
                .value
                .as_ref()
                .and_then(extract_borrow_binding_source);
            if types.is_reference(ty) && borrow_binding.is_none() {
                return fail("reference bindings must use borrow expressions (`&x` or `&mut x`)");
            }
            if let Some((source, _)) = borrow_binding {
                if source == let_stmt.name {
                    return fail("cannot borrow variable from itself in declaration");
                }
            }

            env.define(
                let_stmt.name.clone(),
                Symbol {
                    id: 0,
                    ty,
                    is_mut: let_stmt.is_mut,
                    is_initialized,
                    is_moved: false,
                    is_private: let_stmt.is_private,
                },
            )?;

            if let Some((source_name, is_mut)) = borrow_binding {
                let Some(source_symbol) = env.lookup(source_name).copied() else {
                    return fail(format!("use of undefined variable `{}`", source_name));
                };
                let Some(var_symbol) = env.lookup(&let_stmt.name).copied() else {
                    return fail(format!(
                        "internal error: missing variable `{}` after declaration",
                        let_stmt.name
                    ));
                };
                env.register_ref_binding(var_symbol.id, source_symbol.id, source_name, is_mut)?;
            }

            Ok(())
        }
        Stmt::Assign(assign_stmt) => {
            let Some(symbol) = env.lookup(&assign_stmt.name).copied() else {
                return fail(format!("use of undefined variable `{}`", assign_stmt.name));
            };
            if types.is_reference(symbol.ty) {
                let Some((source, is_mut)) = extract_borrow_binding_source(&assign_stmt.value)
                else {
                    return fail(
                        "reference assignment must use borrow expressions (`&x` or `&mut x`)",
                    );
                };
                if source == assign_stmt.name {
                    return fail("cannot borrow variable from itself in assignment");
                }
                let mut borrows = BorrowContext::default();
                let value_ty = infer_expr_type_with_borrows(
                    &assign_stmt.value,
                    env,
                    types,
                    &mut borrows,
                    ValueUse::ByValue,
                )?;
                if value_ty != symbol.ty {
                    return fail(format!(
                        "type mismatch in assignment to `{}`: expected `{}`, found `{}`",
                        assign_stmt.name,
                        types.name(symbol.ty),
                        types.name(value_ty)
                    ));
                }
                if expr_is_private(&assign_stmt.value, env)? && !symbol.is_private {
                    return fail(format!(
                        "cannot assign private value to non-private variable `{}`",
                        assign_stmt.name
                    ));
                }
                if !symbol.is_mut && symbol.is_initialized {
                    return fail(format!(
                        "cannot assign to immutable variable `{}`",
                        assign_stmt.name
                    ));
                }
                let Some(source_symbol) = env.lookup(source).copied() else {
                    return fail(format!("use of undefined variable `{}`", source));
                };
                env.rebind_ref_binding(symbol.id, source_symbol.id, source, is_mut)?;
                let Some(sym) = env.lookup_mut(&assign_stmt.name) else {
                    return fail(format!(
                        "internal error: missing variable `{}` in assignment",
                        assign_stmt.name
                    ));
                };
                sym.is_initialized = true;
                sym.is_moved = false;
                return Ok(());
            }

            let mut borrows = BorrowContext::default();
            let value_ty = infer_expr_type_with_borrows(
                &assign_stmt.value,
                env,
                types,
                &mut borrows,
                ValueUse::ByValue,
            )?;
            if value_ty != symbol.ty {
                return fail(format!(
                    "type mismatch in assignment to `{}`: expected `{}`, found `{}`",
                    assign_stmt.name,
                    types.name(symbol.ty),
                    types.name(value_ty)
                ));
            }
            if borrows.is_borrowed(symbol.id) {
                return fail(format!(
                    "cannot assign to `{}` while it is borrowed in the same expression",
                    assign_stmt.name
                ));
            }
            if env.has_any_active_borrow(symbol.id) {
                return fail(format!(
                    "cannot assign to `{}` while it has active borrows",
                    assign_stmt.name
                ));
            }
            if expr_is_private(&assign_stmt.value, env)? && !symbol.is_private {
                return fail(format!(
                    "cannot assign private value to non-private variable `{}`",
                    assign_stmt.name
                ));
            }

            if !symbol.is_mut && symbol.is_initialized {
                return fail(format!(
                    "cannot assign to immutable variable `{}`",
                    assign_stmt.name
                ));
            }

            let Some(sym) = env.lookup_mut(&assign_stmt.name) else {
                return fail(format!(
                    "internal error: missing variable `{}` in assignment",
                    assign_stmt.name
                ));
            };
            sym.is_initialized = true;
            sym.is_moved = false;

            Ok(())
        }
        Stmt::FieldAssign(assign_stmt) => {
            let Some(symbol) = env.lookup(&assign_stmt.object).copied() else {
                return fail(format!(
                    "use of undefined variable `{}`",
                    assign_stmt.object
                ));
            };

            if !symbol.is_initialized {
                return fail(format!(
                    "use of uninitialized variable `{}`",
                    assign_stmt.object
                ));
            }

            if !symbol.is_mut {
                return fail(format!(
                    "cannot assign field on immutable variable `{}`",
                    assign_stmt.object
                ));
            }

            let Type::Struct(struct_name) = types.get(symbol.ty) else {
                return fail(format!(
                    "field assignment expects struct variable, found `{}`",
                    types.name(symbol.ty)
                ));
            };

            let Some(def) = env.structs.get(struct_name) else {
                return fail(format!("unknown struct `{}`", struct_name));
            };

            let Some(expected_field_ty) = def.fields.get(&assign_stmt.field).copied() else {
                return fail(format!(
                    "struct `{}` has no field `{}`",
                    struct_name, assign_stmt.field
                ));
            };

            let mut borrows = BorrowContext::default();
            let value_ty = infer_expr_type_with_borrows(
                &assign_stmt.value,
                env,
                types,
                &mut borrows,
                ValueUse::ByValue,
            )?;
            if value_ty != expected_field_ty {
                return fail(format!(
                    "field assignment type mismatch for `{}.{}`: expected `{}`, found `{}`",
                    assign_stmt.object,
                    assign_stmt.field,
                    types.name(expected_field_ty),
                    types.name(value_ty)
                ));
            }
            if borrows.is_borrowed(symbol.id) {
                return fail(format!(
                    "cannot assign field on `{}` while it is borrowed in the same expression",
                    assign_stmt.object
                ));
            }
            if env.has_any_active_borrow(symbol.id) {
                return fail(format!(
                    "cannot assign field on `{}` while it has active borrows",
                    assign_stmt.object
                ));
            }
            if expr_is_private(&assign_stmt.value, env)? && !symbol.is_private {
                return fail(format!(
                    "cannot assign private value to public field `{}.{}`",
                    assign_stmt.object, assign_stmt.field
                ));
            }

            Ok(())
        }
        Stmt::Expr(expr) => {
            let mut borrows = BorrowContext::default();
            let _ =
                infer_expr_type_with_borrows(expr, env, types, &mut borrows, ValueUse::ByValue)?;
            Ok(())
        }
        Stmt::Print(print_stmt) => {
            let mut borrows = BorrowContext::default();
            let expr_ty = infer_expr_type_with_borrows(
                &print_stmt.expr,
                env,
                types,
                &mut borrows,
                ValueUse::ReadOnly,
            )?;
            if matches!(types.get(expr_ty), Type::Void | Type::Struct(_)) {
                return fail(format!(
                    "cannot print value of type `{}`",
                    types.name(expr_ty)
                ));
            }
            if expr_is_private(&print_stmt.expr, env)? {
                return fail("cannot print private value");
            }
            Ok(())
        }
        Stmt::If(if_stmt) => {
            let mut borrows = BorrowContext::default();
            let cond_ty = infer_expr_type_with_borrows(
                &if_stmt.condition,
                env,
                types,
                &mut borrows,
                ValueUse::ReadOnly,
            )?;
            if !matches!(types.get(cond_ty), Type::Bool) {
                return fail("if condition must be boolean");
            }
            if expr_is_private(&if_stmt.condition, env)? {
                return fail("cannot branch on private condition");
            }

            let mut then_env = env.clone();
            check_block(&if_stmt.then_block, &mut then_env, types)?;

            let mut else_env = env.clone();
            if let Some(else_block) = &if_stmt.else_block {
                check_block(else_block, &mut else_env, types)?;
            }

            let then_continues =
                analyze_block_flow(&if_stmt.then_block, env.loop_depth > 0).can_complete_normally;
            let else_continues = if let Some(else_block) = &if_stmt.else_block {
                analyze_block_flow(else_block, env.loop_depth > 0).can_complete_normally
            } else {
                true
            };

            match (then_continues, else_continues) {
                // No continuation after this statement; initialization state is irrelevant.
                (false, false) => {}
                // Continuation only through else path.
                (false, true) => copy_definite_initialization(env, &else_env),
                // Continuation only through then path.
                (true, false) => copy_definite_initialization(env, &then_env),
                // Both paths may continue, require definite initialization in both.
                (true, true) => merge_definite_initialization(env, &then_env, &else_env),
            }
            env.saw_return = env.saw_return || then_env.saw_return || else_env.saw_return;
            Ok(())
        }
        Stmt::While(while_stmt) => {
            let mut borrows = BorrowContext::default();
            let cond_ty = infer_expr_type_with_borrows(
                &while_stmt.condition,
                env,
                types,
                &mut borrows,
                ValueUse::ReadOnly,
            )?;
            if !matches!(types.get(cond_ty), Type::Bool) {
                return fail("while condition must be boolean");
            }
            if expr_is_private(&while_stmt.condition, env)? {
                return fail("cannot branch on private condition");
            }

            let mut loop_env = env.clone();
            loop_env.loop_depth += 1;
            let result = check_block(&while_stmt.body, &mut loop_env, types);
            if result.is_ok() && const_bool_value(&while_stmt.condition) != Some(false) {
                merge_possible_moved_from_loop(env, &loop_env);
            }
            env.saw_return = env.saw_return || loop_env.saw_return;
            result
        }
        Stmt::Loop(loop_stmt) => {
            let loop_flow = analyze_block_flow(&loop_stmt.body, true);
            let mut loop_env = env.clone();
            loop_env.loop_depth += 1;
            let result = check_block(&loop_stmt.body, &mut loop_env, types);
            if result.is_ok() && loop_flow.can_break {
                merge_possible_moved_from_loop(env, &loop_env);
            }
            env.saw_return = env.saw_return || loop_env.saw_return;
            result
        }
        Stmt::Break | Stmt::Continue => {
            if env.loop_depth == 0 {
                fail("`break` and `continue` are only valid inside loops")
            } else {
                Ok(())
            }
        }
        Stmt::Return(value) => {
            let Some(expected) = env.current_return else {
                return fail("`return` is only valid inside functions");
            };

            match (types.get(expected), value) {
                (Type::Void, None) => {
                    env.saw_return = true;
                    Ok(())
                }
                (Type::Void, Some(_)) => fail("void function cannot return a value"),
                (_, None) => fail(format!("function must return `{}`", types.name(expected))),
                (_, Some(expr)) => {
                    let mut borrows = BorrowContext::default();
                    let got = infer_expr_type_with_borrows(
                        expr,
                        env,
                        types,
                        &mut borrows,
                        ValueUse::ByValue,
                    )?;
                    if got != expected {
                        return fail(format!(
                            "return type mismatch: expected `{}`, found `{}`",
                            types.name(expected),
                            types.name(got)
                        ));
                    }
                    if expr_is_private(expr, env)? {
                        return fail("cannot return private value");
                    }
                    env.saw_return = true;
                    Ok(())
                }
            }
        }
    }
}

fn consume_identifier_move(
    name: &str,
    symbol: Symbol,
    env: &mut Env,
    types: &TypeTable,
    borrows: &BorrowContext,
) -> NoerResult<()> {
    if types.is_copy_type(symbol.ty) {
        return Ok(());
    }
    if borrows.is_borrowed(symbol.id) {
        return fail(format!(
            "cannot move `{}` while it is borrowed in the same expression",
            name
        ));
    }
    if env.has_any_active_borrow(symbol.id) {
        return fail(format!(
            "cannot move `{}` while it has active borrows",
            name
        ));
    }
    let Some(sym) = env.lookup_mut(name) else {
        return fail(format!(
            "internal error: missing variable `{}` while applying move semantics",
            name
        ));
    };
    sym.is_moved = true;
    Ok(())
}

fn consume_value_receiver(
    expr: &Expr,
    env: &mut Env,
    types: &TypeTable,
    borrows: &BorrowContext,
) -> NoerResult<()> {
    match expr {
        Expr::Ident(name) => {
            let Some(symbol) = env.lookup(name).copied() else {
                return fail(format!("use of undefined variable `{}`", name));
            };
            consume_identifier_move(name, symbol, env, types, borrows)
        }
        Expr::FieldAccess { .. } => {
            fail("moving values out of field access is not supported in this MVP phase")
        }
        Expr::Group(inner) => consume_value_receiver(inner, env, types, borrows),
        _ => Ok(()),
    }
}

fn infer_expr_type_with_borrows(
    expr: &Expr,
    env: &mut Env,
    types: &TypeTable,
    borrows: &mut BorrowContext,
    value_use: ValueUse,
) -> NoerResult<usize> {
    match expr {
        Expr::Int(_) => Ok(0),
        Expr::Float(_) => Ok(1),
        Expr::Bool(_) => Ok(2),
        Expr::String(_) => Ok(3),
        Expr::Ident(name) => {
            let Some(symbol) = env.lookup(name).copied() else {
                return fail(format!("use of undefined variable `{}`", name));
            };
            if !symbol.is_initialized {
                return fail(format!("use of uninitialized variable `{}`", name));
            }
            if symbol.is_moved {
                return fail(format!("use of moved value `{}`", name));
            }
            if env.has_active_mut_borrow(symbol.id) {
                return fail(format!(
                    "cannot use `{}` while it is mutably borrowed",
                    name
                ));
            }
            if matches!(value_use, ValueUse::ByValue) {
                consume_identifier_move(name, symbol, env, types, borrows)?;
            }
            Ok(symbol.ty)
        }
        Expr::StructInit { name, fields } => {
            let Some(def) = env.structs.get(name).cloned() else {
                return fail(format!("unknown struct `{}`", name));
            };

            let mut seen = HashSet::new();
            for field in fields {
                if !seen.insert(field.name.clone()) {
                    return fail(format!(
                        "field `{}` initialized more than once in `{}`",
                        field.name, name
                    ));
                }

                let Some(expected) = def.fields.get(&field.name).copied() else {
                    return fail(format!(
                        "unknown field `{}` for struct `{}`",
                        field.name, name
                    ));
                };

                let got = infer_expr_type_with_borrows(
                    &field.value,
                    env,
                    types,
                    borrows,
                    ValueUse::ByValue,
                )?;
                if got != expected {
                    return fail(format!(
                        "field `{}` for `{}` expected `{}`, found `{}`",
                        field.name,
                        name,
                        types.name(expected),
                        types.name(got)
                    ));
                }
            }

            for field_name in def.fields.keys() {
                if !seen.contains(field_name) {
                    return fail(format!(
                        "missing field `{}` when initializing `{}`",
                        field_name, name
                    ));
                }
            }

            if let Some(id) = find_struct_type_id(types, name) {
                Ok(id)
            } else {
                fail(format!("internal error: missing struct type `{}`", name))
            }
        }
        Expr::Call { callee, args } => {
            let Some(sig) = env.functions.get(callee).cloned() else {
                return fail(format!("call to undefined function `{}`", callee));
            };

            if args.len() != sig.params.len() {
                return fail(format!(
                    "function `{}` expects {} args, got {}",
                    callee,
                    sig.params.len(),
                    args.len()
                ));
            }

            for (idx, (arg, expected)) in args.iter().zip(&sig.params).enumerate() {
                let arg_use = if types.is_reference(*expected) {
                    ValueUse::ReadOnly
                } else {
                    ValueUse::ByValue
                };
                let got = infer_expr_type_with_borrows(arg, env, types, borrows, arg_use)?;
                if got != *expected {
                    return fail(format!(
                        "arg {} for `{}` expected `{}`, found `{}`",
                        idx + 1,
                        callee,
                        types.name(*expected),
                        types.name(got)
                    ));
                }
                if expr_is_private(arg, env)? {
                    return fail(format!(
                        "cannot pass private value to function `{}` arg {}",
                        callee,
                        idx + 1
                    ));
                }
            }

            Ok(sig.return_type)
        }
        Expr::StaticCall {
            target,
            method,
            args,
        } => {
            let Some(def) = env.structs.get(target).cloned() else {
                return fail(format!("unknown struct `{}`", target));
            };

            let Some(sig) = def.methods.get(method).cloned() else {
                return fail(format!("struct `{}` has no method `{}`", target, method));
            };

            if sig.receiver.is_some() {
                return fail(format!(
                    "method `{}.{}` requires instance receiver, use value call",
                    target, method
                ));
            }

            if args.len() != sig.params.len() {
                return fail(format!(
                    "static method `{}` on `{}` expects {} args, got {}",
                    method,
                    target,
                    sig.params.len(),
                    args.len()
                ));
            }

            for (idx, (arg, expected)) in args.iter().zip(&sig.params).enumerate() {
                let arg_use = if types.is_reference(*expected) {
                    ValueUse::ReadOnly
                } else {
                    ValueUse::ByValue
                };
                let got = infer_expr_type_with_borrows(arg, env, types, borrows, arg_use)?;
                if got != *expected {
                    return fail(format!(
                        "arg {} for static method `{}::{}` expected `{}`, found `{}`",
                        idx + 1,
                        target,
                        method,
                        types.name(*expected),
                        types.name(got)
                    ));
                }
                if expr_is_private(arg, env)? {
                    return fail(format!(
                        "cannot pass private value to static method `{}::{}` arg {}",
                        target,
                        method,
                        idx + 1
                    ));
                }
            }

            Ok(sig.return_type)
        }
        Expr::MethodCall {
            object,
            method,
            args,
        } => {
            let object_ty =
                infer_expr_type_with_borrows(object, env, types, borrows, ValueUse::ReadOnly)?;
            let Type::Struct(name) = types.get(object_ty) else {
                return fail("method call expects a struct value");
            };

            let Some(def) = env.structs.get(name).cloned() else {
                return fail(format!("unknown struct `{}`", name));
            };

            let Some(sig) = def.methods.get(method).cloned() else {
                return fail(format!("struct `{}` has no method `{}`", name, method));
            };

            let Some(receiver) = sig.receiver else {
                return fail(format!(
                    "method `{}.{}` is static; call with `{}::{}`",
                    name, method, name, method
                ));
            };

            if matches!(receiver, SelfParamKind::RefMut) {
                ensure_mutable_method_receiver(object, env)?;
            }

            if args.len() != sig.params.len() {
                return fail(format!(
                    "method `{}` on `{}` expects {} args, got {}",
                    method,
                    name,
                    sig.params.len(),
                    args.len()
                ));
            }

            for (idx, (arg, expected)) in args.iter().zip(&sig.params).enumerate() {
                let arg_use = if types.is_reference(*expected) {
                    ValueUse::ReadOnly
                } else {
                    ValueUse::ByValue
                };
                let got = infer_expr_type_with_borrows(arg, env, types, borrows, arg_use)?;
                if got != *expected {
                    return fail(format!(
                        "arg {} for method `{}.{}` expected `{}`, found `{}`",
                        idx + 1,
                        name,
                        method,
                        types.name(*expected),
                        types.name(got)
                    ));
                }
                if expr_is_private(arg, env)? {
                    return fail(format!(
                        "cannot pass private value to method `{}.{}` arg {}",
                        name,
                        method,
                        idx + 1
                    ));
                }
            }

            if matches!(receiver, SelfParamKind::RefMut) {
                if let Expr::Ident(name) = object.as_ref() {
                    let Some(symbol) = env.lookup(name) else {
                        return fail(format!("use of undefined variable `{}`", name));
                    };
                    if borrows.is_borrowed(symbol.id) || env.has_any_active_borrow(symbol.id) {
                        return fail(format!(
                            "cannot call mutable method on `{}` while it is borrowed",
                            name
                        ));
                    }
                }
            } else if matches!(receiver, SelfParamKind::Value) {
                consume_value_receiver(object, env, types, borrows)?;
            }

            Ok(sig.return_type)
        }
        Expr::FieldAccess { object, field } => {
            let object_ty =
                infer_expr_type_with_borrows(object, env, types, borrows, ValueUse::ReadOnly)?;
            let Type::Struct(name) = types.get(object_ty) else {
                return fail("field access expects a struct value");
            };

            let Some(def) = env.structs.get(name) else {
                return fail(format!("unknown struct `{}`", name));
            };

            if let Some(ty) = def.fields.get(field).copied() {
                if matches!(value_use, ValueUse::ByValue) && !types.is_copy_type(ty) {
                    return fail(format!(
                        "moving non-copy field `{}.{}` is not supported in this MVP phase",
                        name, field
                    ));
                }
                Ok(ty)
            } else {
                fail(format!("struct `{}` has no field `{}`", name, field))
            }
        }
        Expr::Group(inner) => infer_expr_type_with_borrows(inner, env, types, borrows, value_use),
        Expr::Unary { op, expr } => match op {
            UnaryOp::Ref | UnaryOp::RefMut => {
                let Expr::Ident(name) = expr.as_ref() else {
                    return fail("borrow expressions currently require an identifier target");
                };

                let Some(symbol) = env.lookup(name).copied() else {
                    return fail(format!("use of undefined variable `{}`", name));
                };
                if !symbol.is_initialized {
                    return fail(format!("use of uninitialized variable `{}`", name));
                }
                if symbol.is_moved {
                    return fail(format!("use of moved value `{}`", name));
                }

                if matches!(op, UnaryOp::RefMut) {
                    if !symbol.is_mut {
                        return fail(format!(
                            "cannot take mutable borrow of immutable variable `{}`",
                            name
                        ));
                    }
                    if env.has_any_active_borrow(symbol.id) {
                        return fail(format!(
                            "cannot take mutable borrow of `{}` because it is already borrowed",
                            name
                        ));
                    }
                    borrows.borrow_mut(symbol.id, name)?;
                    if let Some(id) = find_ref_type_id(types, symbol.ty, true) {
                        Ok(id)
                    } else {
                        fail(format!(
                            "reference type `&mut {}` is not declared in function/method signatures",
                            types.name(symbol.ty)
                        ))
                    }
                } else {
                    if env.has_active_mut_borrow(symbol.id) {
                        return fail(format!(
                            "cannot take shared borrow of `{}` because it is mutably borrowed",
                            name
                        ));
                    }
                    borrows.borrow_shared(symbol.id, name)?;
                    if let Some(id) = find_ref_type_id(types, symbol.ty, false) {
                        Ok(id)
                    } else {
                        fail(format!(
                            "reference type `&{}` is not declared in function/method signatures",
                            types.name(symbol.ty)
                        ))
                    }
                }
            }
            UnaryOp::Deref => {
                let inner =
                    infer_expr_type_with_borrows(expr, env, types, borrows, ValueUse::ReadOnly)?;
                if let Type::Ref { inner, .. } = types.get(inner) {
                    Ok(*inner)
                } else {
                    fail(format!(
                        "cannot dereference non-reference type `{}`",
                        types.name(inner)
                    ))
                }
            }
            UnaryOp::Neg | UnaryOp::Not => {
                let inner =
                    infer_expr_type_with_borrows(expr, env, types, borrows, ValueUse::ReadOnly)?;
                infer_unary_type(*op, inner, types)
            }
        },
        Expr::Binary { left, op, right } => {
            let left_ty =
                infer_expr_type_with_borrows(left, env, types, borrows, ValueUse::ReadOnly)?;
            let right_ty =
                infer_expr_type_with_borrows(right, env, types, borrows, ValueUse::ReadOnly)?;
            infer_binary_type(*op, left_ty, right_ty, types)
        }
    }
}

fn extract_borrow_binding_source(expr: &Expr) -> Option<(&str, bool)> {
    match expr {
        Expr::Unary {
            op: UnaryOp::Ref,
            expr,
        } => {
            if let Expr::Ident(name) = expr.as_ref() {
                Some((name.as_str(), false))
            } else {
                None
            }
        }
        Expr::Unary {
            op: UnaryOp::RefMut,
            expr,
        } => {
            if let Expr::Ident(name) = expr.as_ref() {
                Some((name.as_str(), true))
            } else {
                None
            }
        }
        Expr::Group(inner) => extract_borrow_binding_source(inner),
        _ => None,
    }
}

fn const_bool_value(expr: &Expr) -> Option<bool> {
    match expr {
        Expr::Bool(v) => Some(*v),
        Expr::Group(inner) => const_bool_value(inner),
        _ => None,
    }
}

fn expr_is_private(expr: &Expr, env: &Env) -> NoerResult<bool> {
    match expr {
        Expr::Int(_) | Expr::Float(_) | Expr::Bool(_) | Expr::String(_) => Ok(false),
        Expr::Ident(name) => {
            if let Some(symbol) = env.lookup(name) {
                if symbol.is_initialized {
                    Ok(symbol.is_private)
                } else {
                    fail(format!("use of uninitialized variable `{}`", name))
                }
            } else {
                fail(format!("use of undefined variable `{}`", name))
            }
        }
        Expr::StructInit { fields, .. } => {
            let mut any_private = false;
            for field in fields {
                any_private = any_private || expr_is_private(&field.value, env)?;
            }
            Ok(any_private)
        }
        Expr::Call { args, .. } | Expr::StaticCall { args, .. } => {
            let mut any_private = false;
            for arg in args {
                any_private = any_private || expr_is_private(arg, env)?;
            }
            Ok(any_private)
        }
        Expr::MethodCall { object, args, .. } => {
            let mut any_private = expr_is_private(object, env)?;
            for arg in args {
                any_private = any_private || expr_is_private(arg, env)?;
            }
            Ok(any_private)
        }
        Expr::FieldAccess { object, .. } => expr_is_private(object, env),
        Expr::Group(inner) => expr_is_private(inner, env),
        Expr::Unary { expr, .. } => expr_is_private(expr, env),
        Expr::Binary { left, right, .. } => {
            Ok(expr_is_private(left, env)? || expr_is_private(right, env)?)
        }
    }
}

fn ensure_mutable_method_receiver(object: &Expr, env: &Env) -> NoerResult<()> {
    let Expr::Ident(name) = object else {
        return fail("`&mut self` methods require a mutable variable receiver");
    };

    let Some(symbol) = env.lookup(name) else {
        return fail(format!("use of undefined variable `{}`", name));
    };

    if !symbol.is_mut {
        return fail(format!("method call requires mutable receiver `{}`", name));
    }

    Ok(())
}

fn infer_unary_type(op: UnaryOp, inner: usize, types: &TypeTable) -> NoerResult<usize> {
    match op {
        UnaryOp::Neg => {
            if types.is_numeric(inner) {
                Ok(inner)
            } else {
                fail("unary `-` expects numeric expression")
            }
        }
        UnaryOp::Not => {
            if matches!(types.get(inner), Type::Bool) {
                Ok(inner)
            } else {
                fail("unary `!` expects bool expression")
            }
        }
        UnaryOp::Ref | UnaryOp::RefMut | UnaryOp::Deref => fail(
            "internal error: borrow unary operator should be handled before type inference helper",
        ),
    }
}

fn infer_binary_type(
    op: BinaryOp,
    left: usize,
    right: usize,
    types: &TypeTable,
) -> NoerResult<usize> {
    match op {
        BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div => {
            if !types.is_numeric(left) || !types.is_numeric(right) {
                return fail(format!(
                    "binary operator `{:?}` expects numeric operands",
                    op
                ));
            }

            if matches!(types.get(left), Type::Float) || matches!(types.get(right), Type::Float) {
                Ok(1)
            } else {
                Ok(0)
            }
        }
        BinaryOp::Eq | BinaryOp::Ne => {
            if left == right || (types.is_numeric(left) && types.is_numeric(right)) {
                Ok(2)
            } else {
                fail(format!("cannot compare incompatible types with `{:?}`", op))
            }
        }
        BinaryOp::Lt | BinaryOp::Le | BinaryOp::Gt | BinaryOp::Ge => {
            if types.is_numeric(left) && types.is_numeric(right) {
                Ok(2)
            } else {
                fail(format!(
                    "comparison operator `{:?}` expects numeric operands",
                    op
                ))
            }
        }
        BinaryOp::And | BinaryOp::Or => {
            if matches!(types.get(left), Type::Bool) && matches!(types.get(right), Type::Bool) {
                Ok(2)
            } else {
                fail(format!("logical operator `{:?}` expects bool operands", op))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::parse;

    use super::check;

    #[test]
    fn accepts_struct_init_and_field_access() {
        let source = "struct User { id: int, name: string, } main() { let u = User { id: 1, name: \"Ana\" }; let id = u.id; print(id); }";
        let program = parse(source).expect("parse failed");
        check(&program).expect("sema failed");
    }

    #[test]
    fn accepts_functions_and_calls() {
        let source =
            "fn sum(a: int, b: int) -> int { return a + b; } main() { let x = sum(1, 2); print(x); }";
        let program = parse(source).expect("parse failed");
        check(&program).expect("sema failed");
    }

    #[test]
    fn accepts_methods_and_method_calls() {
        let source = "struct User { score: int, } impl User { fn score_value(&self) -> int { return self.score; } } main() { let u = User { score: 7 }; let v = u.score_value(); print(v); }";
        let program = parse(source).expect("parse failed");
        check(&program).expect("sema failed");
    }

    #[test]
    fn accepts_field_assignment_and_mut_receiver_method() {
        let source = "struct User { score: int, } impl User { fn bump(&mut self, by: int) -> int { self.score = self.score + by; return self.score; } } main() { let mut u = User { score: 1 }; let v = u.bump(2); print(v); print(u.score); }";
        let program = parse(source).expect("parse failed");
        check(&program).expect("sema failed");
    }

    #[test]
    fn accepts_static_method_call() {
        let source = "struct User { name: string, } impl User { fn make(name: string) -> User { return User { name: name }; } } main() { let u = User::make(\"Ana\"); print(u.name); }";
        let program = parse(source).expect("parse failed");
        check(&program).expect("sema failed");
    }

    #[test]
    fn accepts_typed_let_and_expr_statement() {
        let source = "struct Counter { value: int, } impl Counter { fn inc(&mut self) { self.value = self.value + 1; } } main() { let mut c: Counter = Counter { value: 0 }; c.inc(); let value: int = c.value; print(value); }";
        let program = parse(source).expect("parse failed");
        check(&program).expect("sema failed");
    }

    #[test]
    fn accepts_typed_let_without_initializer_then_assign() {
        let source = "main() { let x: int; x = 1; print(x); }";
        let program = parse(source).expect("parse failed");
        check(&program).expect("sema failed");
    }

    #[test]
    fn accepts_mut_typed_let_without_initializer_reassign() {
        let source = "main() { let mut x: int; x = 1; x = 2; print(x); }";
        let program = parse(source).expect("parse failed");
        check(&program).expect("sema failed");
    }

    #[test]
    fn rejects_struct_init_missing_field() {
        let source =
            "struct User { id: int, name: string, } main() { let u = User { id: 1 }; print(1); }";
        let program = parse(source).expect("parse failed");
        assert!(check(&program).is_err());
    }

    #[test]
    fn rejects_unknown_field_access() {
        let source = "struct User { id: int, } main() { let u = User { id: 1 }; print(u.name); }";
        let program = parse(source).expect("parse failed");
        assert!(check(&program).is_err());
    }

    #[test]
    fn rejects_unknown_method_call() {
        let source =
            "struct User { id: int, } main() { let u = User { id: 1 }; let x = u.label(); print(x); }";
        let program = parse(source).expect("parse failed");
        assert!(check(&program).is_err());
    }

    #[test]
    fn rejects_mut_receiver_call_on_immutable_variable() {
        let source = "struct User { score: int, } impl User { fn bump(&mut self) -> int { return self.score; } } main() { let u = User { score: 1 }; let v = u.bump(); print(v); }";
        let program = parse(source).expect("parse failed");
        assert!(check(&program).is_err());
    }

    #[test]
    fn rejects_instance_call_to_static_method() {
        let source = "struct User { id: int, } impl User { fn value() -> int { return 1; } } main() { let u = User { id: 1 }; let v = u.value(); print(v); }";
        let program = parse(source).expect("parse failed");
        assert!(check(&program).is_err());
    }

    #[test]
    fn rejects_static_call_to_instance_method() {
        let source = "struct User { id: int, } impl User { fn value(&self) -> int { return self.id; } } main() { let v = User::value(); print(v); }";
        let program = parse(source).expect("parse failed");
        assert!(check(&program).is_err());
    }

    #[test]
    fn rejects_impl_for_unknown_struct() {
        let source = "impl User { fn value(self) -> int { return 1; } } main() { print(1); }";
        let program = parse(source).expect("parse failed");
        assert!(check(&program).is_err());
    }

    #[test]
    fn rejects_printing_struct_value() {
        let source = "struct User { id: int, } main() { let u = User { id: 1 }; print(u); }";
        let program = parse(source).expect("parse failed");
        assert!(check(&program).is_err());
    }

    #[test]
    fn accepts_mutable_assignment_in_while() {
        let source =
            "main() { let mut i = 0; while (i < 3) { i = i + 1; } print(i); loop { break; } }";
        let program = parse(source).expect("parse failed");
        check(&program).expect("sema failed");
    }

    #[test]
    fn rejects_assignment_to_immutable() {
        let source = "main() { let x = 1; x = 2; }";
        let program = parse(source).expect("parse failed");
        assert!(check(&program).is_err());
    }

    #[test]
    fn rejects_field_assignment_to_immutable_struct_var() {
        let source =
            "struct User { score: int, } main() { let u = User { score: 1 }; u.score = 2; }";
        let program = parse(source).expect("parse failed");
        assert!(check(&program).is_err());
    }

    #[test]
    fn rejects_typed_let_mismatch() {
        let source = "main() { let value: int = \"oops\"; print(1); }";
        let program = parse(source).expect("parse failed");
        assert!(check(&program).is_err());
    }

    #[test]
    fn rejects_use_before_initialization() {
        let source = "main() { let x: int; print(x); }";
        let program = parse(source).expect("parse failed");
        assert!(check(&program).is_err());
    }

    #[test]
    fn accepts_if_branches_that_both_initialize_variable() {
        let source = "main() { let x: int; if (true) { x = 1; } else { x = 2; } print(x); }";
        let program = parse(source).expect("parse failed");
        check(&program).expect("sema failed");
    }

    #[test]
    fn rejects_if_without_else_when_variable_may_stay_uninitialized() {
        let source = "main() { let x: int; if (true) { x = 1; } print(x); }";
        let program = parse(source).expect("parse failed");
        assert!(check(&program).is_err());
    }

    #[test]
    fn rejects_variable_initialized_only_inside_while() {
        let source = "main() { let x: int; while (false) { x = 1; } print(x); }";
        let program = parse(source).expect("parse failed");
        assert!(check(&program).is_err());
    }

    #[test]
    fn rejects_break_outside_loop() {
        let source = "main() { break; }";
        let program = parse(source).expect("parse failed");
        assert!(check(&program).is_err());
    }

    #[test]
    fn rejects_return_outside_function() {
        let source = "main() { return 1; }";
        let program = parse(source).expect("parse failed");
        assert!(check(&program).is_err());
    }

    #[test]
    fn rejects_function_without_required_return() {
        let source = "fn sum(a: int, b: int) -> int { let x = a + b; } main() { print(1); }";
        let program = parse(source).expect("parse failed");
        assert!(check(&program).is_err());
    }

    #[test]
    fn rejects_function_loop_with_break_path_without_return() {
        let source = "fn pick(flag: bool) -> int { loop { if (flag) { break; } return 1; } } main() { print(1); }";
        let program = parse(source).expect("parse failed");
        assert!(check(&program).is_err());
    }

    #[test]
    fn accepts_function_when_if_else_branches_both_return() {
        let source = "fn pick(flag: bool) -> int { if (flag) { return 1; } else { return 2; } } main() { let x = pick(true); print(x); }";
        let program = parse(source).expect("parse failed");
        check(&program).expect("sema failed");
    }

    #[test]
    fn rejects_function_when_if_else_does_not_return_on_all_paths() {
        let source =
            "fn pick(flag: bool) -> int { if (flag) { return 1; } else { print(2); } } main() { print(1); }";
        let program = parse(source).expect("parse failed");
        assert!(check(&program).is_err());
    }

    #[test]
    fn accepts_method_when_if_else_branches_both_return() {
        let source = "struct User { id: int, } impl User { fn value(&self, flag: bool) -> int { if (flag) { return self.id; } else { return 0; } } } main() { let u = User { id: 1 }; let v = u.value(true); print(v); }";
        let program = parse(source).expect("parse failed");
        check(&program).expect("sema failed");
    }

    #[test]
    fn rejects_method_when_if_else_does_not_return_on_all_paths() {
        let source = "struct User { id: int, } impl User { fn value(&self, flag: bool) -> int { if (flag) { return self.id; } else { print(0); } } } main() { print(1); }";
        let program = parse(source).expect("parse failed");
        assert!(check(&program).is_err());
    }

    #[test]
    fn accepts_init_after_if_when_non_continuing_branch_returns() {
        let source = "fn pick(flag: bool) -> int { let x: int; if (flag) { return 1; } else { x = 2; } return x; } main() { let y = pick(false); print(y); }";
        let program = parse(source).expect("parse failed");
        check(&program).expect("sema failed");
    }

    #[test]
    fn accepts_unreachable_code_after_guaranteed_return_without_reporting_init_error() {
        let source = "fn pick(flag: bool) -> int { if (flag) { return 1; } else { return 2; } let x: int; return x; } main() { print(1); }";
        let program = parse(source).expect("parse failed");
        check(&program).expect("sema failed");
    }

    #[test]
    fn rejects_printing_private_value() {
        let source = "main() { private let token = \"abc\"; print(token); }";
        let program = parse(source).expect("parse failed");
        assert!(check(&program).is_err());
    }

    #[test]
    fn rejects_binding_private_value_to_public_variable() {
        let source = "main() { private let token = \"abc\"; let shadow = token; print(1); }";
        let program = parse(source).expect("parse failed");
        assert!(check(&program).is_err());
    }

    #[test]
    fn accepts_binding_private_value_to_private_variable() {
        let source =
            "main() { private let token = \"abc\"; private let shadow = token; print(1); }";
        let program = parse(source).expect("parse failed");
        check(&program).expect("sema failed");
    }

    #[test]
    fn rejects_passing_private_value_to_function_arg() {
        let source = "fn use_token(token: string) { print(1); } main() { private let token = \"abc\"; use_token(token); }";
        let program = parse(source).expect("parse failed");
        assert!(check(&program).is_err());
    }

    #[test]
    fn rejects_returning_private_value_from_function() {
        let source = "fn expose() -> string { private let token = \"abc\"; return token; } main() { print(1); }";
        let program = parse(source).expect("parse failed");
        assert!(check(&program).is_err());
    }

    #[test]
    fn rejects_if_condition_with_private_value() {
        let source = "main() { private let ok = true; if (ok) { print(1); } else { print(0); } }";
        let program = parse(source).expect("parse failed");
        assert!(check(&program).is_err());
    }

    #[test]
    fn rejects_while_condition_with_private_value() {
        let source = "main() { private let mut ok = true; while (ok) { ok = false; } print(1); }";
        let program = parse(source).expect("parse failed");
        assert!(check(&program).is_err());
    }

    #[test]
    fn accepts_unreachable_code_after_break_in_loop_body() {
        let source = "main() { loop { break; let x: int; print(x); } print(1); }";
        let program = parse(source).expect("parse failed");
        check(&program).expect("sema failed");
    }

    #[test]
    fn accepts_ref_param_and_deref_call() {
        let source =
            "fn read(v: &int) -> int { return *v; } main() { let x = 1; let y = read(&x); print(y); }";
        let program = parse(source).expect("parse failed");
        check(&program).expect("sema failed");
    }

    #[test]
    fn accepts_mut_ref_param_call_on_mutable_var() {
        let source = "fn touch(v: &mut int) -> int { return *v; } main() { let mut x = 1; let y = touch(&mut x); print(y); }";
        let program = parse(source).expect("parse failed");
        check(&program).expect("sema failed");
    }

    #[test]
    fn rejects_mut_borrow_of_immutable_variable() {
        let source = "fn touch(v: &mut int) -> int { return *v; } main() { let x = 1; let y = touch(&mut x); print(y); }";
        let program = parse(source).expect("parse failed");
        assert!(check(&program).is_err());
    }

    #[test]
    fn rejects_conflicting_borrows_in_same_call() {
        let source = "fn pair(a: &int, b: &mut int) -> int { return *a; } main() { let mut x = 1; let y = pair(&x, &mut x); print(y); }";
        let program = parse(source).expect("parse failed");
        assert!(check(&program).is_err());
    }

    #[test]
    fn rejects_assignment_while_borrowed_in_same_expression() {
        let source = "fn read(v: &int) -> int { return *v; } main() { let mut x = 1; x = read(&x); print(x); }";
        let program = parse(source).expect("parse failed");
        assert!(check(&program).is_err());
    }

    #[test]
    fn accepts_reference_local_binding_and_deref() {
        let source =
            "fn read(v: &int) -> int { return *v; } main() { let x = 1; let r = &x; let y = *r; print(y); }";
        let program = parse(source).expect("parse failed");
        check(&program).expect("sema failed");
    }

    #[test]
    fn rejects_reference_return_type_in_mvp_phase() {
        let source = "fn view(v: &int) -> &int { return v; } main() { print(1); }";
        let program = parse(source).expect("parse failed");
        assert!(check(&program).is_err());
    }

    #[test]
    fn rejects_mut_borrow_with_active_lexical_shared_borrow() {
        let source =
            "fn touch(v: &mut int) -> int { return *v; } main() { let mut x = 1; let r = &x; let y = touch(&mut x); print(*r); print(y); }";
        let program = parse(source).expect("parse failed");
        assert!(check(&program).is_err());
    }

    #[test]
    fn accepts_mutation_after_reference_scope_ends() {
        let source =
            "main() { let mut x = 1; if (true) { let r = &x; print(*r); } x = 2; print(x); }";
        let program = parse(source).expect("parse failed");
        check(&program).expect("sema failed");
    }

    #[test]
    fn rejects_assignment_with_active_lexical_shared_borrow() {
        let source = "main() { let mut x = 1; let r = &x; x = 2; print(*r); }";
        let program = parse(source).expect("parse failed");
        assert!(check(&program).is_err());
    }

    #[test]
    fn rejects_using_value_while_mutably_borrowed_lexically() {
        let source = "main() { let mut x = 1; let r = &mut x; print(x); print(*r); }";
        let program = parse(source).expect("parse failed");
        assert!(check(&program).is_err());
    }

    #[test]
    fn accepts_reference_variable_reassignment_when_mutable() {
        let source = "main() { let x = 1; let y = 2; let mut r = &x; r = &y; print(*r); }";
        let program = parse(source).expect("parse failed");
        check(&program).expect("sema failed");
    }

    #[test]
    fn rejects_reference_variable_reassignment_when_immutable() {
        let source = "main() { let x = 1; let y = 2; let r = &x; r = &y; print(*r); }";
        let program = parse(source).expect("parse failed");
        assert!(check(&program).is_err());
    }

    #[test]
    fn accepts_rebinding_mut_reference_releases_old_borrow() {
        let source =
            "main() { let mut x = 1; let mut y = 2; let mut r = &mut x; r = &mut y; x = 3; print(x); print(*r); }";
        let program = parse(source).expect("parse failed");
        check(&program).expect("sema failed");
    }

    #[test]
    fn accepts_shadowed_mut_assignment_with_outer_shared_borrow() {
        let source =
            "main() { let mut x = 1; let r = &x; if (true) { let mut x = 2; x = 3; print(x); } print(*r); }";
        let program = parse(source).expect("parse failed");
        check(&program).expect("sema failed");
    }

    #[test]
    fn accepts_shadowed_read_with_outer_mut_borrow() {
        let source =
            "main() { let mut x = 1; let r = &mut x; if (true) { let x = 2; print(x); } print(*r); }";
        let program = parse(source).expect("parse failed");
        check(&program).expect("sema failed");
    }

    #[test]
    fn accepts_copy_type_after_value_binding() {
        let source = "main() { let x = 1; let y = x; print(x); print(y); }";
        let program = parse(source).expect("parse failed");
        check(&program).expect("sema failed");
    }

    #[test]
    fn rejects_use_after_move_in_binding() {
        let source =
            "struct User { id: int, } main() { let u = User { id: 1 }; let v = u; print(v.id); print(u.id); }";
        let program = parse(source).expect("parse failed");
        assert!(check(&program).is_err());
    }

    #[test]
    fn rejects_use_after_move_when_passing_owned_arg() {
        let source =
            "fn consume(name: string) { print(\"ok\"); } main() { let name = \"ana\"; consume(name); print(name); }";
        let program = parse(source).expect("parse failed");
        assert!(check(&program).is_err());
    }

    #[test]
    fn accepts_borrow_arg_without_move() {
        let source = "fn show(v: &string) { print(1); } main() { let name = \"ana\"; show(&name); print(name); }";
        let program = parse(source).expect("parse failed");
        check(&program).expect("sema failed");
    }

    #[test]
    fn rejects_move_while_lexically_borrowed() {
        let source = "struct User { id: int, } fn consume(u: User) -> int { return u.id; } main() { let u = User { id: 1 }; let r = &u; let id = consume(u); print(id); }";
        let program = parse(source).expect("parse failed");
        assert!(check(&program).is_err());
    }

    #[test]
    fn accepts_printing_string_multiple_times() {
        let source = "main() { let name = \"ana\"; print(name); print(name); }";
        let program = parse(source).expect("parse failed");
        check(&program).expect("sema failed");
    }

    #[test]
    fn rejects_use_after_value_receiver_method_move() {
        let source = "struct User { id: int, } impl User { fn into_id(self) -> int { return self.id; } } main() { let u = User { id: 1 }; let id = u.into_id(); print(id); print(u.id); }";
        let program = parse(source).expect("parse failed");
        assert!(check(&program).is_err());
    }

    #[test]
    fn rejects_move_with_borrow_in_same_call_expression() {
        let source =
            "fn consume_pair(a: &string, b: string) { print(1); } main() { let s = \"a\"; consume_pair(&s, s); }";
        let program = parse(source).expect("parse failed");
        assert!(check(&program).is_err());
    }

    #[test]
    fn rejects_moving_non_copy_field_value() {
        let source =
            "struct User { name: string, } main() { let u = User { name: \"ana\" }; let n = u.name; print(\"ok\"); }";
        let program = parse(source).expect("parse failed");
        assert!(check(&program).is_err());
    }

    #[test]
    fn accepts_passing_copy_field_value_by_value() {
        let source =
            "struct Point { x: int, } fn take(v: int) -> int { return v; } main() { let p = Point { x: 1 }; let y = take(p.x); print(y); print(p.x); }";
        let program = parse(source).expect("parse failed");
        check(&program).expect("sema failed");
    }

    #[test]
    fn rejects_value_receiver_method_on_field_access() {
        let source = "struct User { id: int, } struct Holder { user: User, } impl User { fn into_id(self) -> int { return self.id; } } main() { let h = Holder { user: User { id: 1 } }; let id = h.user.into_id(); print(id); }";
        let program = parse(source).expect("parse failed");
        assert!(check(&program).is_err());
    }

    #[test]
    fn rejects_use_after_possible_move_inside_while() {
        let source = "fn consume(v: string) { print(\"ok\"); } main() { let mut run = true; let name = \"ana\"; while (run) { consume(name); run = false; } print(name); }";
        let program = parse(source).expect("parse failed");
        assert!(check(&program).is_err());
    }

    #[test]
    fn accepts_use_after_move_inside_while_false_literal() {
        let source = "fn consume(v: string) { print(\"ok\"); } main() { let name = \"ana\"; while (false) { consume(name); } print(name); }";
        let program = parse(source).expect("parse failed");
        check(&program).expect("sema failed");
    }

    #[test]
    fn rejects_use_after_possible_move_inside_loop_with_break_path() {
        let source = "fn consume(v: string) { print(\"ok\"); } main() { let s = \"ana\"; loop { consume(s); break; } print(s); }";
        let program = parse(source).expect("parse failed");
        assert!(check(&program).is_err());
    }
}
