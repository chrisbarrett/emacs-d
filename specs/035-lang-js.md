# JavaScript & TypeScript Development

JavaScript and TypeScript development with automatic LSP selection based on project type (Deno, Bun, Node).

## Files

| File               | Purpose                                   |
| :----------------- | :---------------------------------------- |
| `init/init-js.el`  | Mode config, LSP selection, file templates|

## External Packages

None (uses built-in js-ts-mode and typescript-ts-mode).

## Dependencies

- +corelib
- +file-templates
- eglot

## Behaviors

### JavaScript Mode Associations

| Pattern         | Mode        |
| :-------------- | :---------- |
| `*.js`          | js-ts-mode  |
| `*.mjs`         | js-ts-mode  |
| `*.cjs`         | js-ts-mode  |

### TypeScript Shebang Detection

Files with shebangs referencing these executors activate `typescript-ts-mode`:
- `deno`
- `node`
- `bun`
- `tsx`

Pattern: `#!.../(deno|node|bun|tsx)` or `#!... (deno|node|bun|tsx) --...`

### Node Modules Read-Only

Directories matching `/node_modules/` are set to read-only via dir-locals.

### Sibling File Rules

Navigate between implementation and test files:

| From Pattern          | To Pattern              |
| :-------------------- | :---------------------- |
| `*.test.ts`           | `*.ts`                  |
| `*.integration.ts`    | `*.ts`                  |
| `*.ts`                | `*.test.ts`             |
| `*.ts`                | `*.integration.ts`      |

### Project Root Markers

Additional markers for TypeScript projects:

| Marker File      | Project Type     |
| :--------------- | :--------------- |
| `nx.json`        | Nx monorepo      |
| `cdk.json`       | AWS CDK          |
| `deno.json`      | Deno             |
| `deno.jsonc`     | Deno             |
| `bun.lockb`      | Bun              |
| `bunfig.toml`    | Bun              |

### Project VC Ignores

`.nx/` added to `project-vc-ignores`.

## LSP Selection

Eglot automatically selects the appropriate language server based on project type.

### Detection Priority

1. **Shebang** - Check file's shebang line
2. **Project marker** - Walk up to find marker files
3. **Default** - Fall back to Node

### Project Type Detection

| Marker File      | Type   |
| :--------------- | :----- |
| `deno.json`      | deno   |
| `deno.jsonc`     | deno   |
| `bun.lockb`      | bun    |
| `bunfig.toml`    | bun    |
| `package.json`   | node   |

### Server Selection

| Project Type | Server                           |
| :----------- | :------------------------------- |
| deno         | `deno lsp`                       |
| bun          | `typescript-language-server`     |
| node         | `typescript-language-server`     |

### Deno LSP Options

```elisp
(:enable t :lint t :unstable t)
```

### Mode Language IDs

| Mode              | Language ID       |
| :---------------- | :---------------- |
| js-mode           | javascript        |
| js-ts-mode        | javascript        |
| tsx-ts-mode       | typescriptreact   |
| typescript-mode   | typescript        |
| typescript-ts-mode| typescript        |

### Eglot Hook

Eglot starts automatically via `typescript-ts-mode-local-vars-hook`.

## API

### Functions

| Function             | Purpose                                    |
| :------------------- | :----------------------------------------- |
| +ts-project-type     | Detect project type (deno/bun/node) by marker files |
| +ts-shebang-type     | Detect type from shebang line              |
| +ts-server-program   | Return appropriate LSP server command      |
| +cdk-project-p       | Predicate for CDK project detection        |

## File Templates

### CDK Templates

Templates apply to `typescript-ts-mode` files in CDK projects:

| Condition                                  | Template            |
| :----------------------------------------- | :------------------ |
| Path contains "construct", not index.ts    | `cdk/construct.eld` |
| Path contains "/stacks/", not index.ts     | `cdk/stack.eld`     |

Note: Templates are defined but may need corresponding template files in `templates/cdk/`.

## Tempel Snippets

### JavaScript Base (js-base-mode, typescript-ts-base-mode)

| Key   | Expansion                         |
| :---- | :-------------------------------- |
| `l`   | `let VAR = VALUE;`                |
| `c`   | `const VAR = VALUE;`              |
| `ec`  | `export const VAR = VALUE;`       |
| `f`   | `const NAME = (ARGS) => BODY;`    |
| `af`  | `const NAME = async (ARGS) => BODY;` |
| `eaf` | `export const NAME = async...`    |
| `fun` | `function NAME(ARGS) { BODY }`    |
| `afun`| `async function NAME(ARGS) { }`   |
| `eafun`| `export async function...`       |
| `for` | `for (const VAR of VALUE) { }`    |
| `r`   | `return VALUE;`                   |
| `i`   | `if (COND) { BODY }`              |
| `ei`  | `else if (COND) { BODY }`         |
| `e`   | `else { BODY }`                   |
| `ie`  | `if (COND) { THEN } else { ELSE }`|
| `ed`  | `export default VALUE;`           |
| `cl`  | `class NAME { BODY }`             |
| `ecl` | `export class NAME { BODY }`      |
| `ds`  | `describe("NAME", () => { })`     |
| `it`  | `it("NAME", () => { })`           |
| `im`  | `import NAME from "PATH";`        |
| `iz`  | `import { z } from "zod";`        |
| `iv`  | `import VError from "verror";`    |
| `try` | `try { } catch (cause) { }`       |
| `sw`  | `switch (EXPR) { case: { } }`     |
| `ca`  | `case VALUE: { }`                 |
| `ctor`| `constructor(ARGS) { super(); }`  |

### TypeScript Base (typescript-ts-base-mode)

| Key    | Expansion                              |
| :----- | :------------------------------------- |
| `t`    | `type NAME = TYPE;`                    |
| `et`   | `export type NAME = TYPE;`             |
| `ns`   | `namespace NAME { }`                   |
| `ens`  | `export namespace NAME { }`            |
| `zo`   | `export const NAME = z.object({ });`   |
| `zi`   | `export type NAME = z.infer<typeof>;`  |
| `zin`  | `export type In = z.input<typeof>;`    |
| `zm`   | Zod model with type and namespace      |
| `verr` | VError subclass definition             |

### CDK Snippets (typescript-ts-base-mode)

| Key        | Expansion                |
| :--------- | :----------------------- |
| `stack`    | CDK Stack class          |
| `construct`| CDK Construct class      |

## Testable Properties

1. js-ts-mode activates for `*.js` files
2. js-ts-mode activates for `*.mjs` files
3. typescript-ts-mode activates for deno shebang
4. node_modules directories are read-only
5. find-sibling-rules contains `.test.ts` pattern
6. `nx.json` is in project-vc-extra-root-markers
7. +ts-project-type returns 'deno for deno.json projects
8. +ts-project-type returns 'node for package.json projects
9. eglot-server-programs contains typescript-ts-mode entry
