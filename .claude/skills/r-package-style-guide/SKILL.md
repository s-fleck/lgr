---
name: r-script-style-guide
description: >
  Enforce repository standards for R scripts. Use when creating or editing R
  scripts in this repository to apply naming, structure, API/ES/db patterns,
  and style conventions (pipes, env defaults, output, formatting).
user-invocable: true
disable-model-invocation: false
---

# R Style Guide Skill

Apply the repository R script conventions consistently when generating or modifying R code.

## When To Use

- Creating a new R script in a package.
- Refactoring an existing script to repository style.
- Reviewing script style conformance.

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
- **Indentation**: 2 spaces
- **Formatter**: Apply air formatter to new scripts
- **Environment default**: "qa" for safety; change to "prod" only when needed
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

## API Calls (httr2)

```r
response <- request(build_url("wikifolios/query")) |>
  req_method("POST") |>
  req_body_json(request_body) |>
  req_perform()

result <- resp_body_json(response)
```

## Persistence

- **Default**: Save as `.rds`
- **CSV**: Only if explicitly requested
