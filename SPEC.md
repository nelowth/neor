# Noer SPEC (MVP v0.1 draft)

## 1. Objetivo
Noer é uma linguagem segura por design, com sintaxe simples, ownership sem fricção inicial e privacidade por padrão via marcação explícita. O compilador é escrito em Rust e, no MVP, transpila para Rust.

Filosofia:
- Escreva código seguro sem dor de cabeça.
- Proteja dados automaticamente.
- Tenha erros claros e acionáveis.

## 2. Escopo do MVP (v0.1)
- Binário CLI: `noer`
- Comandos: `new`, `build`, `run`, `check`, `fmt`
- Frontend mínimo de linguagem:
  - `main() { ... }`
  - `let` com inferência
  - `let` com anotação opcional de tipo (`let x: int = 1`) e init tardia tipada (`let x: int; x = 1`)
  - atribuição para variáveis mutáveis (`x = expr;`)
  - referências básicas em tipos/parâmetros (`&T`, `&mut T`)
  - borrow/deref em expressão (`&x`, `&mut x`, `*x`)
  - funções (`fn nome(param: tipo, ...) -> tipo`) com `return`
  - chamadas de função (`foo(1, 2)`) também como statement (`foo(1, 2);`)
  - `struct` com campos tipados
  - inicialização de struct (`User { id: 1, name: "Ana" }`)
  - acesso a campo (`u.name`)
  - atribuição em campo (`u.score = 10`)
  - `impl` com métodos (`fn nome(self, ...) -> tipo`, `fn nome(&self, ...)`, `fn nome(&mut self, ...)`, `fn novo(...)`)
  - chamada de método (`u.nome(...)`) e chamada estática (`Tipo::novo(...)`)
  - expressões aritméticas (`+`, `-`, `*`, `/`, parênteses)
  - comparações e lógica (`==`, `!=`, `<`, `<=`, `>`, `>=`, `&&`, `||`, `!`)
  - `if/else`
  - `while`, `loop`, `break`, `continue`
  - `print(...)`
- Backend MVP:
  - Transpilação para Rust
  - Compilação com `rustc`

## 3. Tipos iniciais
- `int`
- `float`
- `bool`
- `string`

No MVP inicial, tipos são inferidos pelo Rust no transpile.

## 3.1 Regras de inicialização definitiva (MVP atual)
- `let x: T;` cria variável tipada ainda não inicializada.
- Usar variável antes de inicialização é erro semântico.
- Em `if/else`, uma variável só é considerada inicializada após o bloco se ambos os ramos a inicializarem.
- Se um ramo de `if/else` garante `return`, apenas o ramo que continua influencia a inicialização após o `if`.
- `if` sem `else` não garante inicialização fora do bloco.
- Inicialização apenas dentro de `while`/`loop` não é considerada garantida fora do loop.

## 3.2 Regras de retorno (MVP atual)
- Funções e métodos com retorno diferente de `void` devem retornar em todos os caminhos de execução.
- `if/else` conta como retorno garantido apenas quando ambos os ramos garantem `return`.
- Código após um `return` garantido no mesmo bloco é tratado como inalcançável no checker.
- `loop` só conta como caminho de retorno garantido quando não há caminho de saída por `break`.
- Código após `break`/`continue` garantidos no mesmo bloco também é tratado como inalcançável no checker.

## 3.3 Regras de privacidade (MVP atual)
- `private let x = ...` marca `x` como dado sensível.
- Valor privado não pode ser:
  - impresso com `print(...)`;
  - retornado em `return ...`;
  - usado como condição de `if`/`while`;
  - passado como argumento para função/método/chamada estática;
  - armazenado em variável/campo não-`private`.
- Fluxo privado para privado é permitido (ex.: `private let b = a;`).

## 4. Regras de ownership (MVP -> fases seguintes)
- Move por padrão.
- Borrow explícito com `&` e `&mut` já disponível em escopo inicial.
- Lifetime explícito fica fora do v0.1.

## 4.1 Limites atuais de borrow checker (MVP atual)
- Conflitos de borrow são checados no mesmo statement e por escopo léxico (ex.: `foo(&x, &mut x)` é rejeitado; mutação com borrow ativo também).
- Armazenar referências em `let` local é permitido (ex.: `let r = &x;`, `let m = &mut x;`).
- Não é permitido retorno por referência nesta fase.
- Reatribuição de variável de referência é permitida apenas quando a variável de referência é `mut` e recebe novo borrow explícito (`r = &y` ou `r = &mut y`).
- Não é permitido campo de `struct` por referência nesta fase.

## 4.2 Regras atuais de move (MVP atual)
- Tipos `Copy` no checker atual: `int`, `float`, `bool`, `&T`, `&mut T`.
- Tipos movidos por padrão: `string` e `struct`.
- `let y = x`, atribuição por valor, `return x`, argumentos por valor e métodos com receiver `self` movem valores não-`Copy`.
- Uso após move gera erro semântico (`use of moved value`).
- Não é permitido mover valor com borrow ativo (no mesmo statement ou por borrow léxico ativo).
- `move` de campo não-`Copy` via acesso de campo (`obj.campo`) ainda não é suportado no MVP (sem partial move).
- Moves ocorridos dentro de `while/loop` com caminho de continuação passam a contaminar o estado pós-loop (evita `use after move` após loop).
- Exceção: `while(false)` literal não propaga move do corpo.
- `print(...)` e condições de `if/while` tratam expressões como leitura (não consomem o valor).

## 5. Privacidade por design
- Variáveis com `private` serão tratadas como sensíveis.
- MVP já aplica bloqueios semânticos de vazamento.
- Cifragem automática no armazenamento permanece como fase seguinte (runtime/stdlib/backend).

## 6. Sintaxe base
Bloco principal:

```noer
main() {
  let x = 42;
  print(x);
}
```

## 7. Exemplos (10)
1. Hello world
```noer
main() {
  print("Hello Noer");
}
```

2. Variável e inferência
```noer
main() {
  let n = 42;
  print(n);
}
```

3. String
```noer
main() {
  let name = "Noer";
  print(name);
}
```

4. Float
```noer
main() {
  let pi = 3.14;
  print(pi);
}
```

5. Boolean
```noer
main() {
  let ok = true;
  print(ok);
}
```

6. Função simples
```noer
fn sum(a: int, b: int) -> int {
  return a + b;
}

main() {
  let total = sum(2, 3);
  print(total);
}
```

7. Borrow imutável (fase 4)
```noer
fn show(v: &string) {
  print(v);
}

main() {
  let name = "ana";
  show(&name);
}
```

8. Borrow mutável (fase 4)
```noer
fn grow(v: &mut int) {
  *v = *v + 1;
}

main() {
  let mut n = 1;
  grow(&mut n);
  print(n);
}
```

9. Dado privado (fase 4/5)
```noer
main() {
  private let token = "super-secret";
  print("token salvo com proteção automática");
}
```

10. Struct + método com receiver (fase 3/4)
```noer
struct User {
  id: int,
  name: string,
}

impl User {
  fn new(id: int, name: string) -> User {
    return User { id: id, name: name };
  }
  fn label(&self) -> string {
    return self.name;
  }
}

main() {
  let u = User::new(1, "Lia");
  print(u.label());
}
```

## 8. Roadmap de implementação técnica
- Fase 1: SPEC + exemplos
- Fase 2: workspace + CLI
- Fase 3: lexer/parser/AST
- Fase 4: semântica + type checker + privacidade
- Fase 5: stdlib + pacotes
- Fase 6: backend próprio (Cranelift/LLVM)
- Fase 7: tooling, LSP, release

## 9. Critérios de pronto do MVP
- `noer new demo` cria projeto base
- `noer build src/main.noer` gera binário
- `noer run src/main.noer` executa o programa
- `noer check src/main.noer` valida parser + semântica sem gerar binário
- `noer fmt` aplica formatação básica
