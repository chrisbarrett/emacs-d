# lang-nix

Nix language support with Tree-sitter syntax, LSP, and format-on-save.

## Files

| Path                          | Purpose                           |
| :---------------------------- | :-------------------------------- |
| `init/init-nix.el`            | Mode, LSP, formatter, project     |
| `file-templates/flake.eld`    | Skeleton template for flake.nix   |

## Dependencies

- **Built-in:** project, json-ts-mode
- **External:** nix-ts-mode, eglot, apheleia
- **Feature:** core (+corelib, +dirlocals-set), templates (+file-templates)

## Behaviors

### Mode Associations

| Pattern       | Mode         |
| :------------ | :----------- |
| `*.nix`       | nix-ts-mode  |
| `/flake.lock` | json-ts-mode |

### Read-only Protection

The Nix store is protected from accidental edits:

| Pattern      | Setting                 |
| :----------- | :---------------------- |
| `/nix/store/`| `read-only-mode` via dirlocals |

### LSP Integration

- **When:** `nix-ts-mode-local-vars-hook`
- **Action:** `eglot-ensure` starts nil or nixd LSP server

### Formatting

- **Formatter:** nixpkgs-fmt via apheleia
- **Scope:** Buffer-local setting in nix-ts-mode

### Project Detection

- **Root marker:** `flake.nix` added to `project-vc-extra-root-markers`
- **Effect:** Directories with flake.nix are recognized as project roots

### File Template

The flake.nix template provides a starter structure:

```nix
{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; };
      in {
        name = "<directory-name>";
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [ _ ];
        };
      });
}
```

- `<directory-name>` is derived from the parent directory
- Cursor positioned at buildInputs list

## API

### Variables

| Variable                        | Type   | Purpose                  |
| :------------------------------ | :----- | :----------------------- |
| `project-vc-extra-root-markers` | list   | Includes "flake.nix"     |

### Auto Mode Entries

| Pattern            | Mode          |
| :----------------- | :------------ |
| `"\\.nix\\'"`      | nix-ts-mode   |
| `"/flake.lock$"`   | json-ts-mode  |

## Testable Properties

1. Opening `*.nix` file activates nix-ts-mode
2. Opening `/flake.lock` activates json-ts-mode
3. Opening file under `/nix/store/` enables read-only-mode
4. eglot-ensure is called on nix-ts-mode-local-vars-hook
5. apheleia-formatter is set to nixpkgs-fmt in nix-ts-mode
6. project-vc-extra-root-markers contains "flake.nix"
7. Creating `flake.nix` inserts template with directory name
