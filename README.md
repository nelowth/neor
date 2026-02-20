# Noer

Noer is a secure-by-design language project.

Current state:
- Rust workspace with `noerc` and `noer-std`
- CLI commands: `new`, `build`, `run`, `check`, `fmt`
- Frontend pipeline: lexer + parser + AST + semantic checks
- Conditionals: `if/else` with boolean/comparison operators
- Loops and control flow: `while`, `loop`, `break`, `continue`
- Mutable assignment with semantic validation
- Typed `let` annotations, including delayed init (`let x: int; x = ...;`)
- Flow-sensitive definite init: values assigned only in one `if` branch or only inside loops are not considered definitely initialized outside
- Non-`void` functions/methods require return on all control-flow paths
- Control-flow aware reachability: statements after guaranteed `return`/`break`/`continue` in the same block are treated as unreachable
- `private` enforcement in sema: private values cannot be printed, returned, used as branch conditions, passed as call args, or assigned to non-private storage
- Initial borrow support: `&T`, `&mut T`, `&x`, `&mut x`, `*x` (lexical-scope + statement conflict checks, local ref bindings, mutable ref rebind)
- Functions: `fn`, typed params, return type, `return`, call expressions
- Structs: declarations, typed fields, init literals, field access, field assignment
- `impl` methods with `self`, `&self`, `&mut self`, static methods, and calls (`u.label()`, `User::new(...)`)
- MVP transpiler: `.noer -> .rs -> rustc`
- Initial language spec in `SPEC.md`

## Quickstart

Build CLI:

```bash
cargo build -p noerc
```

Run example:

```bash
cargo run -p noerc -- run examples/hello.noer
cargo run -p noerc -- run examples/math_private.noer
cargo run -p noerc -- run examples/if_guard.noer
cargo run -p noerc -- run examples/loops_control.noer
cargo run -p noerc -- run examples/functions_calls.noer
cargo run -p noerc -- run examples/borrows_basic.noer
cargo run -p noerc -- run examples/structs_models.noer
cargo run -p noerc -- run examples/methods_impl.noer
cargo run -p noerc -- run examples/methods_mutating.noer
cargo run -p noerc -- run examples/methods_static.noer
cargo run -p noerc -- run examples/typed_calls.noer
cargo run -p noerc -- run examples/typed_uninit.noer
```

Check only (no binary):

```bash
cargo run -p noerc -- check examples/math_private.noer
```

Create new project:

```bash
cargo run -p noerc -- new demo
cargo run -p noerc -- run demo/src/main.noer
```
