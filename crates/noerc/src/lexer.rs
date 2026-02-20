use crate::error::{fail, NoerResult};

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Struct,
    Impl,
    Fn,
    Return,
    Main,
    Let,
    Mut,
    Private,
    Print,
    If,
    Else,
    While,
    Loop,
    Break,
    Continue,
    TyInt,
    TyFloat,
    TyBool,
    TyString,
    TyVoid,
    True,
    False,
    Ident(String),
    Int(i64),
    Float(f64),
    String(String),
    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,
    Colon,
    ColonColon,
    Dot,
    Arrow,
    Equal,
    Semicolon,
    Plus,
    Minus,
    Star,
    Slash,
    Ampersand,
    Bang,
    EqEq,
    NotEq,
    Less,
    LessEq,
    Greater,
    GreaterEq,
    AndAnd,
    OrOr,
    Eof,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub line: usize,
    pub column: usize,
}

pub fn lex(source: &str) -> NoerResult<Vec<Token>> {
    let mut lexer = Lexer::new(source);
    let mut out = Vec::new();

    while !lexer.is_at_end() {
        lexer.skip_whitespace_and_comments();
        if lexer.is_at_end() {
            break;
        }

        let line = lexer.line;
        let column = lexer.column;
        let ch = lexer.peek().unwrap_or('\0');

        let kind = match ch {
            '(' => {
                lexer.advance();
                TokenKind::LParen
            }
            ')' => {
                lexer.advance();
                TokenKind::RParen
            }
            '{' => {
                lexer.advance();
                TokenKind::LBrace
            }
            '}' => {
                lexer.advance();
                TokenKind::RBrace
            }
            ',' => {
                lexer.advance();
                TokenKind::Comma
            }
            ':' => {
                lexer.advance();
                if lexer.match_char(':') {
                    TokenKind::ColonColon
                } else {
                    TokenKind::Colon
                }
            }
            '.' => {
                lexer.advance();
                TokenKind::Dot
            }
            '=' => {
                lexer.advance();
                if lexer.match_char('=') {
                    TokenKind::EqEq
                } else {
                    TokenKind::Equal
                }
            }
            ';' => {
                lexer.advance();
                TokenKind::Semicolon
            }
            '+' => {
                lexer.advance();
                TokenKind::Plus
            }
            '-' => {
                lexer.advance();
                if lexer.match_char('>') {
                    TokenKind::Arrow
                } else {
                    TokenKind::Minus
                }
            }
            '*' => {
                lexer.advance();
                TokenKind::Star
            }
            '/' => {
                lexer.advance();
                TokenKind::Slash
            }
            '!' => {
                lexer.advance();
                if lexer.match_char('=') {
                    TokenKind::NotEq
                } else {
                    TokenKind::Bang
                }
            }
            '<' => {
                lexer.advance();
                if lexer.match_char('=') {
                    TokenKind::LessEq
                } else {
                    TokenKind::Less
                }
            }
            '>' => {
                lexer.advance();
                if lexer.match_char('=') {
                    TokenKind::GreaterEq
                } else {
                    TokenKind::Greater
                }
            }
            '&' => {
                lexer.advance();
                if lexer.match_char('&') {
                    TokenKind::AndAnd
                } else {
                    TokenKind::Ampersand
                }
            }
            '|' => {
                lexer.advance();
                if lexer.match_char('|') {
                    TokenKind::OrOr
                } else {
                    return fail(format!("unexpected character `|` at {}:{}", line, column));
                }
            }
            '"' => TokenKind::String(lexer.lex_string(line, column)?),
            c if c.is_ascii_digit() => lexer.lex_number()?,
            c if is_ident_start(c) => lexer.lex_identifier_or_keyword(),
            _ => {
                return fail(format!(
                    "unexpected character `{}` at {}:{}",
                    ch, line, column
                ));
            }
        };

        out.push(Token { kind, line, column });
    }

    out.push(Token {
        kind: TokenKind::Eof,
        line: lexer.line,
        column: lexer.column,
    });

    Ok(out)
}

struct Lexer {
    chars: Vec<char>,
    index: usize,
    line: usize,
    column: usize,
}

impl Lexer {
    fn new(source: &str) -> Self {
        Self {
            chars: source.chars().collect(),
            index: 0,
            line: 1,
            column: 1,
        }
    }

    fn is_at_end(&self) -> bool {
        self.index >= self.chars.len()
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.index).copied()
    }

    fn peek_next(&self) -> Option<char> {
        self.chars.get(self.index + 1).copied()
    }

    fn advance(&mut self) -> Option<char> {
        let ch = self.peek()?;
        self.index += 1;
        if ch == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
        Some(ch)
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.peek() == Some(expected) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn skip_whitespace_and_comments(&mut self) {
        loop {
            match self.peek() {
                Some(ch) if ch.is_whitespace() => {
                    self.advance();
                }
                Some('/') if self.peek_next() == Some('/') => {
                    while let Some(ch) = self.peek() {
                        self.advance();
                        if ch == '\n' {
                            break;
                        }
                    }
                }
                _ => break,
            }
        }
    }

    fn lex_string(&mut self, start_line: usize, start_col: usize) -> NoerResult<String> {
        let mut out = String::new();

        self.advance();

        while let Some(ch) = self.peek() {
            match ch {
                '"' => {
                    self.advance();
                    return Ok(out);
                }
                '\\' => {
                    self.advance();
                    let Some(esc) = self.peek() else {
                        return fail(format!(
                            "unterminated escape sequence at {}:{}",
                            start_line, start_col
                        ));
                    };
                    match esc {
                        'n' => out.push('\n'),
                        't' => out.push('\t'),
                        '"' => out.push('"'),
                        '\\' => out.push('\\'),
                        _ => {
                            return fail(format!(
                                "unsupported escape `\\{}` at {}:{}",
                                esc, self.line, self.column
                            ));
                        }
                    }
                    self.advance();
                }
                _ => {
                    out.push(ch);
                    self.advance();
                }
            }
        }

        fail(format!(
            "unterminated string literal starting at {}:{}",
            start_line, start_col
        ))
    }

    fn lex_number(&mut self) -> NoerResult<TokenKind> {
        let start = self.index;
        let mut seen_dot = false;

        while let Some(ch) = self.peek() {
            if ch == '.' {
                if seen_dot {
                    break;
                }
                seen_dot = true;
                self.advance();
                continue;
            }

            if ch.is_ascii_digit() {
                self.advance();
                continue;
            }

            break;
        }

        let num: String = self.chars[start..self.index].iter().collect();

        if seen_dot {
            let parsed = num
                .parse::<f64>()
                .map_err(|e| format!("invalid float literal `{}`: {}", num, e))?;
            Ok(TokenKind::Float(parsed))
        } else {
            let parsed = num
                .parse::<i64>()
                .map_err(|e| format!("invalid int literal `{}`: {}", num, e))?;
            Ok(TokenKind::Int(parsed))
        }
    }

    fn lex_identifier_or_keyword(&mut self) -> TokenKind {
        let start = self.index;
        while let Some(ch) = self.peek() {
            if is_ident_continue(ch) {
                self.advance();
            } else {
                break;
            }
        }

        let ident: String = self.chars[start..self.index].iter().collect();
        match ident.as_str() {
            "struct" => TokenKind::Struct,
            "impl" => TokenKind::Impl,
            "fn" => TokenKind::Fn,
            "return" => TokenKind::Return,
            "main" => TokenKind::Main,
            "let" => TokenKind::Let,
            "mut" => TokenKind::Mut,
            "private" => TokenKind::Private,
            "print" => TokenKind::Print,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "while" => TokenKind::While,
            "loop" => TokenKind::Loop,
            "break" => TokenKind::Break,
            "continue" => TokenKind::Continue,
            "int" => TokenKind::TyInt,
            "float" => TokenKind::TyFloat,
            "bool" => TokenKind::TyBool,
            "string" => TokenKind::TyString,
            "void" => TokenKind::TyVoid,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            _ => TokenKind::Ident(ident),
        }
    }
}

fn is_ident_start(ch: char) -> bool {
    ch.is_ascii_alphabetic() || ch == '_'
}

fn is_ident_continue(ch: char) -> bool {
    ch.is_ascii_alphanumeric() || ch == '_'
}

#[cfg(test)]
mod tests {
    use super::{lex, TokenKind};

    #[test]
    fn lexes_core_tokens() {
        let tokens = lex("main() { let x = 1; print(x); }").expect("lex failed");
        assert!(matches!(tokens[0].kind, TokenKind::Main));
        assert!(matches!(tokens[3].kind, TokenKind::LBrace));
        assert!(matches!(tokens[4].kind, TokenKind::Let));
        assert!(matches!(tokens[tokens.len() - 1].kind, TokenKind::Eof));
    }

    #[test]
    fn lexes_private_and_string_escape() {
        let tokens = lex("main() { private let k = \"a\\n\"; }").expect("lex failed");
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::Private)));
        assert!(tokens
            .iter()
            .any(|t| matches!(t.kind, TokenKind::String(_))));
    }

    #[test]
    fn lexes_if_and_boolean_ops() {
        let tokens = lex(
            "main() { if (a >= 1 && b != 0) { print(true); } else { print(false); } while (a < 10) { break; } loop { continue; } }",
        )
        .expect("lex failed");
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::If)));
        assert!(tokens
            .iter()
            .any(|t| matches!(t.kind, TokenKind::GreaterEq)));
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::AndAnd)));
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::NotEq)));
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::Else)));
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::While)));
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::Loop)));
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::Break)));
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::Continue)));
    }

    #[test]
    fn lexes_function_signature_tokens() {
        let tokens = lex("fn sum(a: int, b: int) -> int { return a + b; }").expect("lex failed");
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::Fn)));
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::Colon)));
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::Comma)));
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::Arrow)));
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::Return)));
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::TyInt)));
    }

    #[test]
    fn lexes_struct_and_field_access_tokens() {
        let tokens = lex(
            "struct User { id: int, name: string, } main() { let u = User { id: 1, name: \"Ana\" }; print(u.name); }",
        )
        .expect("lex failed");
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::Struct)));
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::Dot)));
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::TyString)));
    }

    #[test]
    fn lexes_impl_and_method_call_tokens() {
        let tokens = lex(
            "struct User { name: string, } impl User { fn make(name: string) -> User { return User { name: name }; } fn label(&self) -> string { return self.name; } fn touch(&mut self) -> int { return 1; } } main() { let mut u = User::make(\"Ana\"); print(u.label()); print(u.touch()); }",
        )
        .expect("lex failed");
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::Impl)));
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::Fn)));
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::Dot)));
        assert!(tokens
            .iter()
            .any(|t| matches!(t.kind, TokenKind::ColonColon)));
        assert!(tokens
            .iter()
            .any(|t| matches!(t.kind, TokenKind::Ampersand)));
        assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::Mut)));
    }
}
