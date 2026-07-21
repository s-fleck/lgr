---
name: r-package-development
description: R package development with devtools, testthat, and roxygen2. Use when the user is working on an R package, running tests, writing documentation, or building package infrastructure.
metadata:
  author: Stefan Fleck
  version: "1.2"
---

# R Package Development

## Commands

| Task                                     | Command                                                                  |
| ---------------------------------------- | ------------------------------------------------------------------------ |
| Load package and run code                | `Rscript -e "devtools::load_all(); <code>"`                              |
| Run all tests                            | `Rscript -e "devtools::test()"`                                          |
| Run tests for files matching `<name>`    | `Rscript -e "devtools::test(filter = '^<name>')"`                        |
| Run tests for `R/<name>.R`               | `Rscript -e "devtools::test_active_file('R/<name>.R')"`                  |
| Run single test `<desc>` in `R/<name>.R` | `Rscript -e "devtools::test_active_file('R/<name>.R', desc = '<desc>')"` |
| Regenerate documentation                 | `Rscript -e "devtools::document()"`                                      |
| Check pkgdown site                       | `Rscript -e "pkgdown::check_pkgdown()"`                                  |
| Run R CMD check                          | `Rscript -e "devtools::check()"`                                         |
| Format code                              | `air format .`                                                           |

## Coding

- Run `air format .` after generating any code.
- Use the base pipe `|>`, not the magrittr pipe `%>%`.
- Anonymous functions: use `\() ...` for single-line; use `function() { ... }` otherwise.

## Testing

- Tests for `R/<name>.R` go in `tests/testthat/test-<name>.R`.
- Write a test for every new code change.
- Place new tests next to similar existing tests.
- Keep tests minimal with few comments.
- Prefer specific expectations over `expect_true()` / `expect_false()` for better failure messages.
- For errors: use `expect_snapshot(error = TRUE)`. For warnings: use `expect_snapshot()`. Do **not** use `expect_error()` or `expect_warning()`.

## Documentation

- Export every user-facing function and add roxygen2 documentation.
- Wrap roxygen comments at 80 characters.
- Do not add roxygen documentation to internal functions.
- Run `devtools::document()` after every roxygen2 change.

## pkgdown documentation

- if `_pkgdown.yml` exists:
  - When adding a new public documentation topic, also add it to `_pkgdown.yml`
  - Run `pkgdown::check_pkgdown()` to verify all topics appear in the reference index
- otherwise ignore all instructions related to `pkgdown`

## NEWS.md

- Add a bullet for every user-facing change. Skip bullets for minor doc changes or internal refactors.
- Each bullet: one or more sentences, no line wraps, mentions the related issue in parentheses.
- Start bullets that relate to a specific function with the function name.
- Order bullets alphabetically by function name; unrelated bullets go first.

## Imports & NAMESPACE

- Reference non-base-package functions with `::`, unless already imported via `@import` or `@importFrom`.
- Do **not** add or remove `@import` / `@importFrom` directives — leave that to the user.
- Never edit `NAMESPACE` directly. You may freely delete and regenerate with `devtools::document()` any time you see fit.

## Dependencies

- Avoid adding new dependencies. If a new package could help, suggest it to the user instead of using it automatically.
- Run `usethis::use_tidy_description()` after modifying `DESCRIPTION`.
