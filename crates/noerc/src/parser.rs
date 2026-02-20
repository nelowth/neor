use crate::{
    ast::{
        AssignStmt, BinaryOp, Block, Expr, FieldAssignStmt, FunctionDecl, IfStmt, ImplDecl,
        LetStmt, LoopStmt, MethodDecl, Param, PrintStmt, Program, SelfParamKind, Stmt, StructDecl,
        StructFieldDecl, StructFieldInit, TypeName, UnaryOp, WhileStmt,
    },
    error::{fail, NoerResult},
    lexer::{lex, Token, TokenKind},
};

pub fn parse(source: &str) -> NoerResult<Program> {
    let tokens = lex(source)?;
    Parser::new(tokens).parse_program()
}

struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0 }
    }

    fn parse_program(mut self) -> NoerResult<Program> {
        let mut structs = Vec::new();
        let mut impls = Vec::new();
        let mut functions = Vec::new();

        loop {
            if self.check_struct() {
                structs.push(self.parse_struct()?);
                continue;
            }
            if self.check_impl() {
                impls.push(self.parse_impl()?);
                continue;
            }
            if self.check_fn() {
                functions.push(self.parse_function()?);
                continue;
            }
            break;
        }

        self.consume_main()?;
        self.consume_lparen()?;
        self.consume_rparen()?;
        let main = self.parse_block()?;
        self.consume_eof()?;

        Ok(Program {
            structs,
            impls,
            functions,
            main,
        })
    }

    fn parse_struct(&mut self) -> NoerResult<StructDecl> {
        self.consume_struct_kw()?;
        let name = self.consume_ident()?;
        self.consume_lbrace()?;

        let mut fields = Vec::new();
        if !self.check_rbrace() {
            loop {
                let field_name = self.consume_ident()?;
                self.consume_colon()?;
                let ty = self.parse_type_name()?;
                fields.push(StructFieldDecl {
                    name: field_name,
                    ty,
                });

                if self.consume_if_comma() {
                    if self.check_rbrace() {
                        break;
                    }
                    continue;
                }

                break;
            }
        }

        self.consume_rbrace()?;
        Ok(StructDecl { name, fields })
    }

    fn parse_impl(&mut self) -> NoerResult<ImplDecl> {
        self.consume_impl_kw()?;
        let target = self.consume_ident()?;
        self.consume_lbrace()?;

        let mut methods = Vec::new();
        while !self.check_rbrace() {
            if !self.check_fn() {
                return self.error_here("expected `fn` inside `impl` block");
            }
            methods.push(self.parse_method()?);
        }

        self.consume_rbrace()?;
        Ok(ImplDecl { target, methods })
    }

    fn parse_method(&mut self) -> NoerResult<MethodDecl> {
        self.consume_fn_kw()?;
        let name = self.consume_ident()?;
        if name == "main" {
            return self.error_here("`main` is reserved for entrypoint block");
        }

        self.consume_lparen()?;
        let (receiver, params) = self.parse_method_params()?;
        self.consume_rparen()?;

        let return_type = if self.consume_if_arrow() {
            self.parse_type_name()?
        } else {
            TypeName::Void
        };

        let body = self.parse_block()?;

        Ok(MethodDecl {
            name,
            receiver,
            params,
            return_type,
            body,
        })
    }

    fn parse_function(&mut self) -> NoerResult<FunctionDecl> {
        self.consume_fn_kw()?;
        let name = self.consume_ident()?;
        if name == "main" {
            return self.error_here("`main` is reserved for entrypoint block");
        }

        self.consume_lparen()?;
        let params = self.parse_params()?;
        self.consume_rparen()?;

        let return_type = if self.consume_if_arrow() {
            self.parse_type_name()?
        } else {
            TypeName::Void
        };

        let body = self.parse_block()?;

        Ok(FunctionDecl {
            name,
            params,
            return_type,
            body,
        })
    }

    fn parse_params(&mut self) -> NoerResult<Vec<Param>> {
        let mut params = Vec::new();

        if self.check_rparen() {
            return Ok(params);
        }

        loop {
            let name = self.consume_ident()?;
            self.consume_colon()?;
            let ty = self.parse_type_name()?;
            params.push(Param { name, ty });

            if self.consume_if_comma() {
                continue;
            }

            break;
        }

        Ok(params)
    }

    fn parse_method_params(&mut self) -> NoerResult<(Option<SelfParamKind>, Vec<Param>)> {
        if self.check_rparen() {
            return Ok((None, Vec::new()));
        }

        if let Some(receiver) = self.parse_receiver()? {
            if self.consume_if_comma() {
                return Ok((Some(receiver), self.parse_params()?));
            }
            return Ok((Some(receiver), Vec::new()));
        }

        Ok((None, self.parse_params()?))
    }

    fn parse_receiver(&mut self) -> NoerResult<Option<SelfParamKind>> {
        if self.check_ident_named("self") {
            self.advance();
            return Ok(Some(SelfParamKind::Value));
        }

        if self.consume_if_ampersand() {
            let is_mut = self.consume_if_mut();
            if !self.check_ident_named("self") {
                return self.error_here("expected `self` after `&` in method receiver");
            }
            self.advance();
            return Ok(Some(if is_mut {
                SelfParamKind::RefMut
            } else {
                SelfParamKind::Ref
            }));
        }

        Ok(None)
    }

    fn parse_type_name(&mut self) -> NoerResult<TypeName> {
        if self.consume_if_ampersand() {
            let inner = if self.consume_if_mut() {
                TypeName::RefMut(Box::new(self.parse_type_name()?))
            } else {
                TypeName::Ref(Box::new(self.parse_type_name()?))
            };
            return Ok(inner);
        }

        let ty = match &self.current().kind {
            TokenKind::TyInt => {
                self.advance();
                TypeName::Int
            }
            TokenKind::TyFloat => {
                self.advance();
                TypeName::Float
            }
            TokenKind::TyBool => {
                self.advance();
                TypeName::Bool
            }
            TokenKind::TyString => {
                self.advance();
                TypeName::String
            }
            TokenKind::TyVoid => {
                self.advance();
                TypeName::Void
            }
            TokenKind::Ident(name) => {
                let name = name.clone();
                self.advance();
                TypeName::Custom(name)
            }
            _ => {
                return self.error_here(
                    "expected type (`int`, `float`, `bool`, `string`, `void`, `&T`, `&mut T` or custom)",
                );
            }
        };
        Ok(ty)
    }

    fn parse_block(&mut self) -> NoerResult<Block> {
        self.consume_lbrace()?;

        let mut statements = Vec::new();
        while !self.check_rbrace() {
            if self.is_eof() {
                return self.error_here("expected `}` to close block");
            }
            statements.push(self.parse_statement()?);
        }

        self.consume_rbrace()?;
        Ok(Block { statements })
    }

    fn parse_statement(&mut self) -> NoerResult<Stmt> {
        if self.match_private_or_let() {
            return self.parse_let();
        }
        if self.check_field_assignment() {
            return self.parse_field_assign();
        }
        if self.check_ident_assignment() {
            return self.parse_assign();
        }
        if self.check_return() {
            return self.parse_return();
        }
        if self.check_print() {
            return self.parse_print();
        }
        if self.check_if() {
            return self.parse_if();
        }
        if self.check_while() {
            return self.parse_while();
        }
        if self.check_loop() {
            return self.parse_loop();
        }
        if self.check_break() {
            return self.parse_break();
        }
        if self.check_continue() {
            return self.parse_continue();
        }
        if self.can_start_expression() {
            return self.parse_expr_stmt();
        }

        self.error_here(
            "expected statement (`let`, assignment, field assignment, expression statement, `return`, `print`, `if`, `while`, `loop`, `break`, `continue`)",
        )
    }

    fn parse_let(&mut self) -> NoerResult<Stmt> {
        let is_private = self.consume_if_private();
        self.consume_let()?;
        let is_mut = self.consume_if_mut();
        let name = self.consume_ident()?;
        let declared_type = if self.consume_if_colon() {
            Some(self.parse_type_name()?)
        } else {
            None
        };
        let value = if self.consume_if_equal() {
            Some(self.parse_expression()?)
        } else {
            None
        };

        if value.is_none() && declared_type.is_none() {
            return self.error_here("`let` without initializer requires explicit type");
        }

        self.consume_semicolon()?;

        Ok(Stmt::Let(LetStmt {
            name,
            is_mut,
            is_private,
            declared_type,
            value,
        }))
    }

    fn parse_assign(&mut self) -> NoerResult<Stmt> {
        let name = self.consume_ident()?;
        self.consume_equal()?;
        let value = self.parse_expression()?;
        self.consume_semicolon()?;

        Ok(Stmt::Assign(AssignStmt { name, value }))
    }

    fn parse_field_assign(&mut self) -> NoerResult<Stmt> {
        let object = self.consume_ident()?;
        self.consume_dot()?;
        let field = self.consume_ident()?;
        self.consume_equal()?;
        let value = self.parse_expression()?;
        self.consume_semicolon()?;

        Ok(Stmt::FieldAssign(FieldAssignStmt {
            object,
            field,
            value,
        }))
    }

    fn parse_expr_stmt(&mut self) -> NoerResult<Stmt> {
        let expr = self.parse_expression()?;
        self.consume_semicolon()?;
        Ok(Stmt::Expr(expr))
    }

    fn parse_return(&mut self) -> NoerResult<Stmt> {
        self.consume_return_kw()?;
        if self.check_semicolon() {
            self.consume_semicolon()?;
            return Ok(Stmt::Return(None));
        }

        let expr = self.parse_expression()?;
        self.consume_semicolon()?;
        Ok(Stmt::Return(Some(expr)))
    }

    fn parse_print(&mut self) -> NoerResult<Stmt> {
        self.consume_print()?;
        self.consume_lparen()?;
        let expr = self.parse_expression()?;
        self.consume_rparen()?;
        self.consume_semicolon()?;
        Ok(Stmt::Print(PrintStmt { expr }))
    }

    fn parse_if(&mut self) -> NoerResult<Stmt> {
        self.consume_if_kw()?;
        self.consume_lparen()?;
        let condition = self.parse_expression()?;
        self.consume_rparen()?;
        let then_block = self.parse_block()?;

        let else_block = if self.consume_if_else_kw() {
            Some(self.parse_block()?)
        } else {
            None
        };

        Ok(Stmt::If(IfStmt {
            condition,
            then_block,
            else_block,
        }))
    }

    fn parse_while(&mut self) -> NoerResult<Stmt> {
        self.consume_while_kw()?;
        self.consume_lparen()?;
        let condition = self.parse_expression()?;
        self.consume_rparen()?;
        let body = self.parse_block()?;
        Ok(Stmt::While(WhileStmt { condition, body }))
    }

    fn parse_loop(&mut self) -> NoerResult<Stmt> {
        self.consume_loop_kw()?;
        let body = self.parse_block()?;
        Ok(Stmt::Loop(LoopStmt { body }))
    }

    fn parse_break(&mut self) -> NoerResult<Stmt> {
        self.consume_break_kw()?;
        self.consume_semicolon()?;
        Ok(Stmt::Break)
    }

    fn parse_continue(&mut self) -> NoerResult<Stmt> {
        self.consume_continue_kw()?;
        self.consume_semicolon()?;
        Ok(Stmt::Continue)
    }

    fn parse_expression(&mut self) -> NoerResult<Expr> {
        self.parse_logical_or()
    }

    fn parse_logical_or(&mut self) -> NoerResult<Expr> {
        let mut expr = self.parse_logical_and()?;

        while self.consume_if_or_or() {
            let right = self.parse_logical_and()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op: BinaryOp::Or,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn parse_logical_and(&mut self) -> NoerResult<Expr> {
        let mut expr = self.parse_equality()?;

        while self.consume_if_and_and() {
            let right = self.parse_equality()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op: BinaryOp::And,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn parse_equality(&mut self) -> NoerResult<Expr> {
        let mut expr = self.parse_comparison()?;

        loop {
            let op = if self.consume_if_eq_eq() {
                Some(BinaryOp::Eq)
            } else if self.consume_if_not_eq() {
                Some(BinaryOp::Ne)
            } else {
                None
            };

            let Some(op) = op else {
                break;
            };

            let right = self.parse_comparison()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn parse_comparison(&mut self) -> NoerResult<Expr> {
        let mut expr = self.parse_additive()?;

        loop {
            let op = if self.consume_if_less() {
                Some(BinaryOp::Lt)
            } else if self.consume_if_less_eq() {
                Some(BinaryOp::Le)
            } else if self.consume_if_greater() {
                Some(BinaryOp::Gt)
            } else if self.consume_if_greater_eq() {
                Some(BinaryOp::Ge)
            } else {
                None
            };

            let Some(op) = op else {
                break;
            };

            let right = self.parse_additive()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn parse_additive(&mut self) -> NoerResult<Expr> {
        let mut expr = self.parse_multiplicative()?;

        loop {
            let op = if self.consume_if_plus() {
                Some(BinaryOp::Add)
            } else if self.consume_if_minus() {
                Some(BinaryOp::Sub)
            } else {
                None
            };

            let Some(op) = op else {
                break;
            };

            let right = self.parse_multiplicative()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn parse_multiplicative(&mut self) -> NoerResult<Expr> {
        let mut expr = self.parse_unary()?;

        loop {
            let op = if self.consume_if_star() {
                Some(BinaryOp::Mul)
            } else if self.consume_if_slash() {
                Some(BinaryOp::Div)
            } else {
                None
            };

            let Some(op) = op else {
                break;
            };

            let right = self.parse_unary()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn parse_unary(&mut self) -> NoerResult<Expr> {
        if self.consume_if_ampersand() {
            let is_mut = self.consume_if_mut();
            let expr = self.parse_unary()?;
            let op = if is_mut {
                UnaryOp::RefMut
            } else {
                UnaryOp::Ref
            };
            return Ok(Expr::Unary {
                op,
                expr: Box::new(expr),
            });
        }

        if self.consume_if_star() {
            let expr = self.parse_unary()?;
            return Ok(Expr::Unary {
                op: UnaryOp::Deref,
                expr: Box::new(expr),
            });
        }

        if self.consume_if_minus() {
            let expr = self.parse_unary()?;
            return Ok(Expr::Unary {
                op: UnaryOp::Neg,
                expr: Box::new(expr),
            });
        }

        if self.consume_if_bang() {
            let expr = self.parse_unary()?;
            return Ok(Expr::Unary {
                op: UnaryOp::Not,
                expr: Box::new(expr),
            });
        }

        self.parse_postfix()
    }

    fn parse_postfix(&mut self) -> NoerResult<Expr> {
        let mut expr = self.parse_primary_base()?;

        loop {
            if self.consume_if_lparen() {
                let args = self.parse_call_args()?;
                self.consume_rparen()?;

                let Expr::Ident(callee) = expr else {
                    return self.error_here("only identifier calls are supported");
                };
                expr = Expr::Call { callee, args };
                continue;
            }

            if self.consume_if_dot() {
                let member = self.consume_ident()?;
                if self.consume_if_lparen() {
                    let args = self.parse_call_args()?;
                    self.consume_rparen()?;
                    expr = Expr::MethodCall {
                        object: Box::new(expr),
                        method: member,
                        args,
                    };
                } else {
                    expr = Expr::FieldAccess {
                        object: Box::new(expr),
                        field: member,
                    };
                }
                continue;
            }

            break;
        }

        Ok(expr)
    }

    fn parse_primary_base(&mut self) -> NoerResult<Expr> {
        let token = self.current().clone();
        match token.kind {
            TokenKind::Int(v) => {
                self.advance();
                Ok(Expr::Int(v))
            }
            TokenKind::Float(v) => {
                self.advance();
                Ok(Expr::Float(v))
            }
            TokenKind::True => {
                self.advance();
                Ok(Expr::Bool(true))
            }
            TokenKind::False => {
                self.advance();
                Ok(Expr::Bool(false))
            }
            TokenKind::String(ref s) => {
                self.advance();
                Ok(Expr::String(s.clone()))
            }
            TokenKind::Ident(ref name) => {
                let ident = name.clone();
                self.advance();

                if self.consume_if_colon_colon() {
                    let method = self.consume_ident()?;
                    self.consume_lparen()?;
                    let args = self.parse_call_args()?;
                    self.consume_rparen()?;
                    Ok(Expr::StaticCall {
                        target: ident,
                        method,
                        args,
                    })
                } else if self.consume_if_lbrace() {
                    let fields = self.parse_struct_init_fields()?;
                    self.consume_rbrace()?;
                    Ok(Expr::StructInit {
                        name: ident,
                        fields,
                    })
                } else {
                    Ok(Expr::Ident(ident))
                }
            }
            TokenKind::LParen => {
                self.advance();
                let inner = self.parse_expression()?;
                self.consume_rparen()?;
                Ok(Expr::Group(Box::new(inner)))
            }
            _ => self.error_here("expected expression"),
        }
    }

    fn parse_struct_init_fields(&mut self) -> NoerResult<Vec<StructFieldInit>> {
        let mut fields = Vec::new();

        if self.check_rbrace() {
            return Ok(fields);
        }

        loop {
            let name = self.consume_ident()?;
            self.consume_colon()?;
            let value = self.parse_expression()?;
            fields.push(StructFieldInit { name, value });

            if self.consume_if_comma() {
                if self.check_rbrace() {
                    break;
                }
                continue;
            }

            break;
        }

        Ok(fields)
    }

    fn parse_call_args(&mut self) -> NoerResult<Vec<Expr>> {
        let mut args = Vec::new();

        if self.check_rparen() {
            return Ok(args);
        }

        loop {
            args.push(self.parse_expression()?);
            if self.consume_if_comma() {
                continue;
            }
            break;
        }

        Ok(args)
    }

    fn current(&self) -> &Token {
        self.tokens.get(self.pos).unwrap_or_else(|| {
            self.tokens
                .last()
                .expect("parser requires at least one token")
        })
    }

    fn peek_kind(&self, offset: usize) -> Option<&TokenKind> {
        self.tokens.get(self.pos + offset).map(|t| &t.kind)
    }

    fn advance(&mut self) {
        if !self.is_eof() {
            self.pos += 1;
        }
    }

    fn is_eof(&self) -> bool {
        matches!(self.current().kind, TokenKind::Eof)
    }

    fn error_here<T>(&self, message: &str) -> NoerResult<T> {
        let token = self.current();
        fail(format!("{} at {}:{}", message, token.line, token.column))
    }

    fn check_struct(&self) -> bool {
        matches!(self.current().kind, TokenKind::Struct)
    }

    fn check_impl(&self) -> bool {
        matches!(self.current().kind, TokenKind::Impl)
    }

    fn check_fn(&self) -> bool {
        matches!(self.current().kind, TokenKind::Fn)
    }

    fn check_ident_named(&self, expected: &str) -> bool {
        matches!(&self.current().kind, TokenKind::Ident(name) if name == expected)
    }

    fn check_rbrace(&self) -> bool {
        matches!(self.current().kind, TokenKind::RBrace)
    }

    fn check_rparen(&self) -> bool {
        matches!(self.current().kind, TokenKind::RParen)
    }

    fn check_semicolon(&self) -> bool {
        matches!(self.current().kind, TokenKind::Semicolon)
    }

    fn check_print(&self) -> bool {
        matches!(self.current().kind, TokenKind::Print)
    }

    fn check_return(&self) -> bool {
        matches!(self.current().kind, TokenKind::Return)
    }

    fn check_if(&self) -> bool {
        matches!(self.current().kind, TokenKind::If)
    }

    fn check_while(&self) -> bool {
        matches!(self.current().kind, TokenKind::While)
    }

    fn check_loop(&self) -> bool {
        matches!(self.current().kind, TokenKind::Loop)
    }

    fn check_break(&self) -> bool {
        matches!(self.current().kind, TokenKind::Break)
    }

    fn check_continue(&self) -> bool {
        matches!(self.current().kind, TokenKind::Continue)
    }

    fn can_start_expression(&self) -> bool {
        matches!(
            self.current().kind,
            TokenKind::Ident(_)
                | TokenKind::Int(_)
                | TokenKind::Float(_)
                | TokenKind::True
                | TokenKind::False
                | TokenKind::String(_)
                | TokenKind::LParen
                | TokenKind::Minus
                | TokenKind::Bang
                | TokenKind::Ampersand
                | TokenKind::Star
        )
    }

    fn check_ident_assignment(&self) -> bool {
        matches!(self.current().kind, TokenKind::Ident(_))
            && matches!(self.peek_kind(1), Some(TokenKind::Equal))
    }

    fn check_field_assignment(&self) -> bool {
        matches!(self.current().kind, TokenKind::Ident(_))
            && matches!(self.peek_kind(1), Some(TokenKind::Dot))
            && matches!(self.peek_kind(2), Some(TokenKind::Ident(_)))
            && matches!(self.peek_kind(3), Some(TokenKind::Equal))
    }

    fn match_private_or_let(&self) -> bool {
        matches!(self.current().kind, TokenKind::Private | TokenKind::Let)
    }

    fn consume_if_private(&mut self) -> bool {
        if matches!(self.current().kind, TokenKind::Private) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn consume_if_else_kw(&mut self) -> bool {
        if matches!(self.current().kind, TokenKind::Else) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn consume_if_lparen(&mut self) -> bool {
        if matches!(self.current().kind, TokenKind::LParen) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn consume_if_lbrace(&mut self) -> bool {
        if matches!(self.current().kind, TokenKind::LBrace) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn consume_if_dot(&mut self) -> bool {
        if matches!(self.current().kind, TokenKind::Dot) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn consume_if_arrow(&mut self) -> bool {
        if matches!(self.current().kind, TokenKind::Arrow) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn consume_if_comma(&mut self) -> bool {
        if matches!(self.current().kind, TokenKind::Comma) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn consume_if_colon_colon(&mut self) -> bool {
        if matches!(self.current().kind, TokenKind::ColonColon) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn consume_if_colon(&mut self) -> bool {
        if matches!(self.current().kind, TokenKind::Colon) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn consume_if_equal(&mut self) -> bool {
        if matches!(self.current().kind, TokenKind::Equal) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn consume_if_mut(&mut self) -> bool {
        if matches!(self.current().kind, TokenKind::Mut) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn consume_if_ampersand(&mut self) -> bool {
        if matches!(self.current().kind, TokenKind::Ampersand) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn consume_if_plus(&mut self) -> bool {
        if matches!(self.current().kind, TokenKind::Plus) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn consume_if_minus(&mut self) -> bool {
        if matches!(self.current().kind, TokenKind::Minus) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn consume_if_star(&mut self) -> bool {
        if matches!(self.current().kind, TokenKind::Star) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn consume_if_slash(&mut self) -> bool {
        if matches!(self.current().kind, TokenKind::Slash) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn consume_if_bang(&mut self) -> bool {
        if matches!(self.current().kind, TokenKind::Bang) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn consume_if_or_or(&mut self) -> bool {
        if matches!(self.current().kind, TokenKind::OrOr) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn consume_if_and_and(&mut self) -> bool {
        if matches!(self.current().kind, TokenKind::AndAnd) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn consume_if_eq_eq(&mut self) -> bool {
        if matches!(self.current().kind, TokenKind::EqEq) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn consume_if_not_eq(&mut self) -> bool {
        if matches!(self.current().kind, TokenKind::NotEq) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn consume_if_less(&mut self) -> bool {
        if matches!(self.current().kind, TokenKind::Less) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn consume_if_less_eq(&mut self) -> bool {
        if matches!(self.current().kind, TokenKind::LessEq) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn consume_if_greater(&mut self) -> bool {
        if matches!(self.current().kind, TokenKind::Greater) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn consume_if_greater_eq(&mut self) -> bool {
        if matches!(self.current().kind, TokenKind::GreaterEq) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn consume_struct_kw(&mut self) -> NoerResult<()> {
        if matches!(self.current().kind, TokenKind::Struct) {
            self.advance();
            Ok(())
        } else {
            self.error_here("expected `struct`")
        }
    }

    fn consume_impl_kw(&mut self) -> NoerResult<()> {
        if matches!(self.current().kind, TokenKind::Impl) {
            self.advance();
            Ok(())
        } else {
            self.error_here("expected `impl`")
        }
    }

    fn consume_fn_kw(&mut self) -> NoerResult<()> {
        if matches!(self.current().kind, TokenKind::Fn) {
            self.advance();
            Ok(())
        } else {
            self.error_here("expected `fn`")
        }
    }

    fn consume_main(&mut self) -> NoerResult<()> {
        if matches!(self.current().kind, TokenKind::Main) {
            self.advance();
            Ok(())
        } else {
            self.error_here("expected `main`")
        }
    }

    fn consume_let(&mut self) -> NoerResult<()> {
        if matches!(self.current().kind, TokenKind::Let) {
            self.advance();
            Ok(())
        } else {
            self.error_here("expected `let`")
        }
    }

    fn consume_print(&mut self) -> NoerResult<()> {
        if matches!(self.current().kind, TokenKind::Print) {
            self.advance();
            Ok(())
        } else {
            self.error_here("expected `print`")
        }
    }

    fn consume_return_kw(&mut self) -> NoerResult<()> {
        if matches!(self.current().kind, TokenKind::Return) {
            self.advance();
            Ok(())
        } else {
            self.error_here("expected `return`")
        }
    }

    fn consume_if_kw(&mut self) -> NoerResult<()> {
        if matches!(self.current().kind, TokenKind::If) {
            self.advance();
            Ok(())
        } else {
            self.error_here("expected `if`")
        }
    }

    fn consume_while_kw(&mut self) -> NoerResult<()> {
        if matches!(self.current().kind, TokenKind::While) {
            self.advance();
            Ok(())
        } else {
            self.error_here("expected `while`")
        }
    }

    fn consume_loop_kw(&mut self) -> NoerResult<()> {
        if matches!(self.current().kind, TokenKind::Loop) {
            self.advance();
            Ok(())
        } else {
            self.error_here("expected `loop`")
        }
    }

    fn consume_break_kw(&mut self) -> NoerResult<()> {
        if matches!(self.current().kind, TokenKind::Break) {
            self.advance();
            Ok(())
        } else {
            self.error_here("expected `break`")
        }
    }

    fn consume_continue_kw(&mut self) -> NoerResult<()> {
        if matches!(self.current().kind, TokenKind::Continue) {
            self.advance();
            Ok(())
        } else {
            self.error_here("expected `continue`")
        }
    }

    fn consume_ident(&mut self) -> NoerResult<String> {
        if let TokenKind::Ident(name) = self.current().kind.clone() {
            self.advance();
            Ok(name)
        } else {
            self.error_here("expected identifier")
        }
    }

    fn consume_lparen(&mut self) -> NoerResult<()> {
        if matches!(self.current().kind, TokenKind::LParen) {
            self.advance();
            Ok(())
        } else {
            self.error_here("expected `(`")
        }
    }

    fn consume_rparen(&mut self) -> NoerResult<()> {
        if matches!(self.current().kind, TokenKind::RParen) {
            self.advance();
            Ok(())
        } else {
            self.error_here("expected `)`")
        }
    }

    fn consume_lbrace(&mut self) -> NoerResult<()> {
        if matches!(self.current().kind, TokenKind::LBrace) {
            self.advance();
            Ok(())
        } else {
            self.error_here("expected `{`")
        }
    }

    fn consume_rbrace(&mut self) -> NoerResult<()> {
        if matches!(self.current().kind, TokenKind::RBrace) {
            self.advance();
            Ok(())
        } else {
            self.error_here("expected `}`")
        }
    }

    fn consume_colon(&mut self) -> NoerResult<()> {
        if matches!(self.current().kind, TokenKind::Colon) {
            self.advance();
            Ok(())
        } else {
            self.error_here("expected `:`")
        }
    }

    fn consume_dot(&mut self) -> NoerResult<()> {
        if matches!(self.current().kind, TokenKind::Dot) {
            self.advance();
            Ok(())
        } else {
            self.error_here("expected `.`")
        }
    }

    fn consume_equal(&mut self) -> NoerResult<()> {
        if matches!(self.current().kind, TokenKind::Equal) {
            self.advance();
            Ok(())
        } else {
            self.error_here("expected `=`")
        }
    }

    fn consume_semicolon(&mut self) -> NoerResult<()> {
        if matches!(self.current().kind, TokenKind::Semicolon) {
            self.advance();
            Ok(())
        } else {
            self.error_here("expected `;`")
        }
    }

    fn consume_eof(&mut self) -> NoerResult<()> {
        if self.is_eof() {
            Ok(())
        } else {
            self.error_here("unexpected tokens after program end")
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{BinaryOp, Expr, SelfParamKind, Stmt, TypeName};

    use super::parse;

    #[test]
    fn parses_precedence_correctly() {
        let source = "main() { let x = 1 + 2 * 3; print(x); }";
        let program = parse(source).expect("parse failed");

        let Stmt::Let(let_stmt) = &program.main.statements[0] else {
            panic!("expected let statement");
        };

        let Some(Expr::Binary { op, right, .. }) = &let_stmt.value else {
            panic!("expected binary expression");
        };

        assert_eq!(*op, BinaryOp::Add);
        assert!(matches!(
            **right,
            Expr::Binary {
                op: BinaryOp::Mul,
                ..
            }
        ));
    }

    #[test]
    fn parses_struct_decl_init_and_field_access() {
        let source = "struct User { id: int, name: string, } main() { let u = User { id: 1, name: \"Ana\" }; print(u.name); }";
        let program = parse(source).expect("parse failed");

        assert_eq!(program.structs.len(), 1);
        assert_eq!(program.structs[0].name, "User");
        assert_eq!(program.structs[0].fields.len(), 2);
        assert!(matches!(program.structs[0].fields[1].ty, TypeName::String));

        let Stmt::Let(let_stmt) = &program.main.statements[0] else {
            panic!("expected let statement");
        };
        assert!(matches!(let_stmt.value, Some(Expr::StructInit { .. })));

        let Stmt::Print(print_stmt) = &program.main.statements[1] else {
            panic!("expected print statement");
        };
        assert!(matches!(print_stmt.expr, Expr::FieldAccess { .. }));
    }

    #[test]
    fn parses_impl_method_and_method_call() {
        let source = "struct User { name: string, } impl User { fn label(&self) -> string { return self.name; } } main() { let u = User { name: \"Ana\" }; print(u.label()); }";
        let program = parse(source).expect("parse failed");

        assert_eq!(program.impls.len(), 1);
        assert_eq!(program.impls[0].target, "User");
        assert_eq!(program.impls[0].methods.len(), 1);
        assert_eq!(
            program.impls[0].methods[0].receiver,
            Some(SelfParamKind::Ref)
        );
        assert_eq!(program.impls[0].methods[0].name, "label");

        let Stmt::Print(print_stmt) = &program.main.statements[1] else {
            panic!("expected print statement");
        };
        assert!(matches!(print_stmt.expr, Expr::MethodCall { .. }));
    }

    #[test]
    fn parses_static_method_and_static_call() {
        let source = "struct User { name: string, } impl User { fn make(name: string) -> User { return User { name: name }; } } main() { let u = User::make(\"Ana\"); print(u.name); }";
        let program = parse(source).expect("parse failed");

        assert_eq!(program.impls.len(), 1);
        assert_eq!(program.impls[0].methods.len(), 1);
        assert_eq!(program.impls[0].methods[0].receiver, None);
        assert_eq!(program.impls[0].methods[0].name, "make");

        let Stmt::Let(let_stmt) = &program.main.statements[0] else {
            panic!("expected let statement");
        };
        assert!(matches!(let_stmt.value, Some(Expr::StaticCall { .. })));
    }

    #[test]
    fn parses_field_assignment() {
        let source = "struct User { score: int, } main() { let mut u = User { score: 1 }; u.score = u.score + 1; print(u.score); }";
        let program = parse(source).expect("parse failed");
        assert_eq!(program.main.statements.len(), 3);
        assert!(matches!(program.main.statements[1], Stmt::FieldAssign(_)));
    }

    #[test]
    fn parses_typed_let_and_expr_statement() {
        let source = "struct Counter { value: int, } impl Counter { fn inc(&mut self) { self.value = self.value + 1; } } main() { let mut c: Counter = Counter { value: 0 }; c.inc(); let v: int = c.value; print(v); }";
        let program = parse(source).expect("parse failed");
        assert_eq!(program.main.statements.len(), 4);

        let Stmt::Let(first_let) = &program.main.statements[0] else {
            panic!("expected first let");
        };
        assert!(matches!(first_let.declared_type, Some(TypeName::Custom(_))));

        assert!(matches!(program.main.statements[1], Stmt::Expr(_)));

        let Stmt::Let(second_let) = &program.main.statements[2] else {
            panic!("expected second let");
        };
        assert!(matches!(second_let.declared_type, Some(TypeName::Int)));
    }

    #[test]
    fn parses_typed_let_without_initializer() {
        let source = "main() { let mut x: int; x = 1; print(x); }";
        let program = parse(source).expect("parse failed");
        let Stmt::Let(let_stmt) = &program.main.statements[0] else {
            panic!("expected let statement");
        };
        assert!(matches!(let_stmt.declared_type, Some(TypeName::Int)));
        assert!(let_stmt.value.is_none());
    }

    #[test]
    fn parses_functions_and_call() {
        let source = "fn sum(a: int, b: int) -> int { return a + b; } main() { let total = sum(2, 3); print(total); }";
        let program = parse(source).expect("parse failed");

        assert_eq!(program.functions.len(), 1);
        assert_eq!(program.functions[0].name, "sum");
        let Stmt::Let(let_stmt) = &program.main.statements[0] else {
            panic!("expected let statement");
        };
        assert!(matches!(let_stmt.value, Some(Expr::Call { .. })));
    }

    #[test]
    fn parses_if_else() {
        let source = "main() { let x = 1; if (x >= 1 && true) { print(1); } else { print(0); } }";
        let program = parse(source).expect("parse failed");

        assert_eq!(program.main.statements.len(), 2);
        let Stmt::If(if_stmt) = &program.main.statements[1] else {
            panic!("expected if statement");
        };

        assert!(if_stmt.else_block.is_some());
    }

    #[test]
    fn parses_while_loop_assignment_and_control_flow() {
        let source = "main() { let mut i = 0; while (i < 3) { i = i + 1; if (i == 2) { continue; } } loop { break; } }";
        let program = parse(source).expect("parse failed");
        assert_eq!(program.main.statements.len(), 3);
        assert!(matches!(program.main.statements[1], Stmt::While(_)));
        assert!(matches!(program.main.statements[2], Stmt::Loop(_)));
    }

    #[test]
    fn parses_ref_type_and_deref_expression() {
        let source = "fn read(v: &int) -> int { return *v; } main() { let x = 1; let y = read(&x); print(y); }";
        let program = parse(source).expect("parse failed");
        assert_eq!(program.functions.len(), 1);
        assert!(matches!(
            program.functions[0].params[0].ty,
            TypeName::Ref(_)
        ));

        let Stmt::Return(Some(Expr::Unary { op, .. })) = &program.functions[0].body.statements[0]
        else {
            panic!("expected deref return");
        };
        assert!(matches!(*op, crate::ast::UnaryOp::Deref));
    }

    #[test]
    fn parses_ref_mut_type_and_borrow_expr() {
        let source = "fn touch(v: &mut int) -> int { return *v; } main() { let mut x = 1; let y = touch(&mut x); print(y); }";
        let program = parse(source).expect("parse failed");
        assert_eq!(program.functions.len(), 1);
        assert!(matches!(
            program.functions[0].params[0].ty,
            TypeName::RefMut(_)
        ));

        let Stmt::Let(let_stmt) = &program.main.statements[1] else {
            panic!("expected let with call");
        };
        let Some(Expr::Call { args, .. }) = &let_stmt.value else {
            panic!("expected call expression");
        };
        assert!(matches!(args[0], Expr::Unary { .. }));
    }

    #[test]
    fn rejects_missing_semicolon() {
        let source = "main() { print(1) }";
        assert!(parse(source).is_err());
    }

    #[test]
    fn rejects_untyped_let_without_initializer() {
        let source = "main() { let x; }";
        assert!(parse(source).is_err());
    }
}
