#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub structs: Vec<StructDecl>,
    pub impls: Vec<ImplDecl>,
    pub functions: Vec<FunctionDecl>,
    pub main: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDecl {
    pub name: String,
    pub fields: Vec<StructFieldDecl>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructFieldDecl {
    pub name: String,
    pub ty: TypeName,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImplDecl {
    pub target: String,
    pub methods: Vec<MethodDecl>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MethodDecl {
    pub name: String,
    pub receiver: Option<SelfParamKind>,
    pub params: Vec<Param>,
    pub return_type: TypeName,
    pub body: Block,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SelfParamKind {
    Value,
    Ref,
    RefMut,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDecl {
    pub name: String,
    pub params: Vec<Param>,
    pub return_type: TypeName,
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub name: String,
    pub ty: TypeName,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeName {
    Int,
    Float,
    Bool,
    String,
    Void,
    Ref(Box<TypeName>),
    RefMut(Box<TypeName>),
    Custom(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub statements: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Let(LetStmt),
    Assign(AssignStmt),
    FieldAssign(FieldAssignStmt),
    Expr(Expr),
    Print(PrintStmt),
    If(IfStmt),
    While(WhileStmt),
    Loop(LoopStmt),
    Break,
    Continue,
    Return(Option<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetStmt {
    pub name: String,
    pub is_mut: bool,
    pub is_private: bool,
    pub declared_type: Option<TypeName>,
    pub value: Option<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PrintStmt {
    pub expr: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssignStmt {
    pub name: String,
    pub value: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldAssignStmt {
    pub object: String,
    pub field: String,
    pub value: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfStmt {
    pub condition: Expr,
    pub then_block: Block,
    pub else_block: Option<Block>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileStmt {
    pub condition: Expr,
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LoopStmt {
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Ident(String),
    StructInit {
        name: String,
        fields: Vec<StructFieldInit>,
    },
    Call {
        callee: String,
        args: Vec<Expr>,
    },
    StaticCall {
        target: String,
        method: String,
        args: Vec<Expr>,
    },
    MethodCall {
        object: Box<Expr>,
        method: String,
        args: Vec<Expr>,
    },
    FieldAccess {
        object: Box<Expr>,
        field: String,
    },
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        op: BinaryOp,
        right: Box<Expr>,
    },
    Group(Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructFieldInit {
    pub name: String,
    pub value: Expr,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
    Not,
    Ref,
    RefMut,
    Deref,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Or,
    And,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    Add,
    Sub,
    Mul,
    Div,
}
