# lang-terraform

Terraform and HCL development with specialized formatting, file templates, and error parsing.

## Overview

Provides Terraform and HCL language support with:
- terraform-mode for `.tf` files
- hcl-mode for `.hcl` files (Terragrunt, Packer)
- OpenTofu formatter for Terraform files
- Terragrunt formatter for HCL files
- File templates for Terragrunt configurations
- Tempel snippets for common patterns
- Comprehensive compilation error parsers for Terraform/Terragrunt output

## Files

| File                         | Purpose                                    |
| :--------------------------- | :----------------------------------------- |
| `init/init-terraform.el`     | Mode configuration, formatters             |
| `templates/terraform.eld`    | Terraform snippets                         |
| `templates/hcl.eld`          | HCL/Terragrunt snippets                    |
| `config/mod-compilation.el`  | Error parsers (terraform, terragrunt)      |

## Packages

| Package         | Source    | Purpose                         |
| :-------------- | :-------- | :------------------------------ |
| `hcl-mode`      | external  | Major mode for HCL files        |
| `terraform-mode`| external  | Major mode for Terraform files  |

## Behaviors

### Mode Associations

| Pattern           | Mode            |
| :---------------- | :-------------- |
| `*.hcl`           | `hcl-mode`      |
| `*.tf`            | `terraform-mode`|

### Formatters

Configured via apheleia:

| Mode             | Formatter       | Command                      |
| :--------------- | :-------------- | :--------------------------- |
| `terraform-mode` | `opentofu`      | `tofu fmt -`                 |
| `hcl-mode`       | `terragrunt`    | `terragrunt hcl fmt --stdin` |

Falls back to `hclfmt` if terragrunt unavailable.

### Project Configuration

| Setting                      | Value                    | Purpose                      |
| :--------------------------- | :----------------------- | :--------------------------- |
| `project-vc-ignores`         | `.drift-history.json`    | Ignore Gruntwork Pipelines history |

### File Templates

Templates defined for Terragrunt configuration files:

| Pattern             | Template                      |
| :------------------ | :---------------------------- |
| `terragrunt.hcl`    | `terragrunt/terragrunt.eld`   |
| `root.hcl`          | `terragrunt/root.eld`         |
| `region.hcl`        | `terragrunt/region.eld`       |

### Tempel Snippets

**Terraform mode** (9 snippets):

| Key   | Expands To                                    |
| :---- | :-------------------------------------------- |
| `o`   | `output "NAME" { value = ... }`               |
| `v`   | `variable "NAME" { ... }`                     |
| `r`   | `resource "TYPE" "NAME" { ... }`              |
| `d`   | `data "TYPE" "NAME" { ... }`                  |
| `m`   | `module "NAME" { source = "./NAME" }`         |
| `pol` | IAM policy document + policy resource         |
| `st`  | IAM policy statement block                    |
| `role`| IAM role with assume role policy              |
| `att` | IAM role policy attachment                    |

**HCL mode** (7 snippets):

| Key   | Expands To                                    |
| :---- | :-------------------------------------------- |
| `d`   | Terragrunt `dependency { ... }`               |
| `g`   | Terragrunt `generate { ... }` with heredoc    |
| `u`   | Terragrunt Stack `unit { ... }`               |
| `st`  | Terragrunt Stack `stack { ... }`              |
| `in`  | `inputs = { ... }`                            |
| `inc` | `include "root" { path = ... }`               |
| `l`   | `locals { ... }` (both HCL and Terraform)     |

### Compilation Error Parsers

Nine custom parsers for Terraform/Terragrunt output:

| Parser                    | Pattern                                         | Type   |
| :------------------------ | :---------------------------------------------- | :----- |
| `terraform`               | `│ Error: ...` with `on file line N`            | error  |
| `terragrunt`              | `* Validation failed for unit...` or `file:line,col:` | error |
| `terragrunt-err`          | Timestamped ERROR with file/line location       | error  |
| `terragrunt-info`         | `from/at './path.hcl'` references               | info   |
| `terragrunt-unit-operation`| INFO/WARN/ERROR with `[unit]` prefix           | varies |
| `terragrunt-stack-modules`| `- Module path` references                      | info   |
| `terragrunt-unit-reference`| `Processing unit X from file`                  | info   |
| `tflint`                   | `file:line:col: W/E message`                   | error/warning |
| `trivy-terraform`          | `file (terraform)` references                  | error  |

### Path Transformations

Terragrunt stacks report errors from `.terragrunt-stack/` directory. Transformations navigate to actual source:

| Pattern                    | Transform                       |
| :------------------------- | :------------------------------ |
| `/.terragrunt-stack/`      | `/` (strip intermediate dir)    |
| `/.terragrunt-stack` (dir) | `/terragrunt.stack.hcl`         |

## API

### Functions

None (uses standard package functionality).

### Keybindings

Inherits standard keybindings from hcl-mode and terraform-mode.

## Testable Properties

1. Opening `.tf` file activates `terraform-mode`
2. Opening `.hcl` file activates `hcl-mode`
3. `apheleia-mode-alist` maps `terraform-mode` to `opentofu`
4. `apheleia-mode-alist` maps `hcl-mode` to `(terragrunt hclfmt)`
5. `.drift-history.json` is in `project-vc-ignores`
6. Tempel snippet `r` expands to resource block in terraform-mode
7. Tempel snippet `d` expands to dependency in hcl-mode
8. Terraform compilation errors navigate to correct location
9. Terragrunt compilation errors with timestamps navigate correctly
