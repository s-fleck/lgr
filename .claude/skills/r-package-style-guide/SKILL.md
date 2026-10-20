---
name: r-package-style-guide
description: >
  Code style conventions for R package code. Use when writing or refactoring
  functions in an R package to apply company style rules (pipes, anonymous
  functions, formatting, data.table, prohibited packages).
metadata:
  author: Stefan Fleck
  version: "2.0"
---

# R Package Style Guide

Apply these conventions consistently when generating or modifying code in an R
package. For workflow commands, testing, documentation and NAMESPACE rules see
the `r-package-development` skill.

## When To Use

- Writing a new function in `R/`.
- Refactoring existing package code to company style.
- Reviewing package code for style conformance.

## AI Coding Directives

- **Favor vectorized operations** over loops; prefer functional composition and stateless pipelines
- **Minimize output**: Use `{cli}` package for necessary console output only; avoid excessive logging
- **Show base R solutions first**; suggest helper functions only for genuinely repetitive operations
- **Separate concerns**: Keep data preparation separate from plotting and layout
- **Avoid excessive comments**: Code should be self-explanatory
- **Validate the final code**: Ensure style conformance before finishing.

## Code Conventions

- **Pipe**: Always use `|>`, never `%>%`
- **Anonymous functions**: Use `\(x) x + 1`, not `function(x)`
- **URL building**: Use `sfmisc::url_tidy(...)`
- **Indentation**: 2 spaces
- **Formatter**: Run `air format .` after generating any code
- **Prohibited packages** Never use the following packages: magrittr, elastic
- Do not use `dplyr::case_match()`, use the modern replacement `dplyr::recode_values()` instead

## data.table

**Do not pipe `set*` functions** - they modify by reference:

```r
# Good:
data.table::setnames(iris, sfmisc::pascal_case(names(iris)))

# Bad:
iris |> data.table::setnames(sfmisc::pascal_case(names(iris)))
```
