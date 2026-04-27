---
name: testing-r-packages
description: Best practices for writing R package tests using testthat version 3+. Use when writing, organizing, or improving tests for R packages. Covers test structure, expectations, fixtures, snapshots, mocking, and modern testthat 3 patterns including self-sufficient tests, proper cleanup with withr, and snapshot testing.
metadata:
  author: Garrick Aden-Buie (@gadenbuie)
  version: "2.0"
license: MIT
---

# Testing R Packages with testthat

Modern best practices for R package testing using testthat 3+.

## Quick Agent Checklist

- Each test file is named `test-<source-file>.R` matching the corresponding `R/<source-file>.R`
- Every test uses `test_that("<function> - <scenario> - <expected outcome>", { ... })`
- Follow Arrange / Act / Assert structure with comments
- Test one behaviour per `test_that()` block
- Keep tests deterministic and side-effect free
- Run `devtools::test()` or `testthat::test_check("<pkg>")` to validate

## Initial Setup

Initialize testing with testthat 3rd edition:

```r
usethis::use_testthat(3)
```

This creates `tests/testthat/` directory, adds testthat to `DESCRIPTION` Suggests with `Config/testthat/edition: 3`, and creates `tests/testthat.R`.

**Load the package under test with `library(mypkg)` — never `source()` files directly:**

```r
# In tests/testthat.R
library(testthat)
library(mypkg)
test_check("mypkg")
```

## File Organization

**Mirror package structure:**

- Code in `R/foofy.R` → tests in `tests/testthat/test-foofy.R`
- Use `usethis::use_r("foofy")` and `usethis::use_test("foofy")` to create paired files

**Special files:**

- `helper-*.R` - Helper functions and custom expectations, sourced before tests
- `setup-*.R` - Run during `R CMD check` only, not during `load_all()`
- `fixtures/` - Static test data files accessed via `test_path()`

## Test Structure

Tests follow a three-level hierarchy: **File → Test → Expectation**

### Standard Syntax — Arrange / Act / Assert Pattern

```r
test_that("my_function - valid input - returns expected value", {
  # Arrange
  x <- c(1, 2, 3)

  # Act
  result <- my_function(x)

  # Assert
  expect_equal(result, 6)
})
```

### Test Naming Convention

`"<function_name> - <scenario description> - <expected outcome>"`

**Concrete examples:**

- `"parse_elastic_timestamp - numeric seconds - parses correctly"`
- `"parse_elastic_timestamp - iso8601 with z timezone - parses correctly"`
- `"parse_elastic_timestamp - invalid format - returns na"`
- `"parse_elastic_timestamp - empty vector - returns empty posixct"`

This format reads naturally and describes behavior, not implementation.

## Accessing Internal Functions

Use `:::` to test unexported (non-exported) functions:

```r
result <- mypkg:::internal_helper(x)
```

Do **not** add `@export` just to make a function testable.

## Running Tests

Three scales of testing:

**Micro** (interactive development):

```r
devtools::load_all()
expect_equal(foofy(...), expected)
```

**Mezzo** (single file):

```r
testthat::test_file("tests/testthat/test-foofy.R")
# RStudio: Ctrl/Cmd + Shift + T
```

**Macro** (full suite):

```r
devtools::test()    # Ctrl/Cmd + Shift + T
devtools::check()   # Ctrl/Cmd + Shift + E
```

## Core Expectations

### Expectations — Preferred Patterns

| Goal                 | Expectation                                    |
| -------------------- | ---------------------------------------------- |
| Exact equality       | `expect_equal(result, expected)`               |
| Bit-exact equality   | `expect_identical(result, expected)`           |
| S3 class             | `expect_s3_class(result, "POSIXct")`           |
| S4/R5 class          | `expect_s4_class(result, "ClassName")`         |
| Is `NA`              | `expect_true(is.na(result))`                   |
| Not `NA`             | `expect_false(is.na(result))`                  |
| Vector length        | `expect_length(result, n)`                     |
| Error thrown         | `expect_error(expr, regexp)`                   |
| Error with class     | `expect_error(expr, class = "my_error_class")` |
| No error             | `expect_no_error(expr)`                        |
| Warning thrown       | `expect_warning(expr, regexp)`                 |
| No warning           | `expect_no_warning(expr)`                      |
| Message thrown       | `expect_message(expr, regexp)`                 |
| No message           | `expect_no_message(expr)`                      |
| Pattern match        | `expect_match(text, "pattern")`                |
| Type check           | `expect_type(obj, "list")`                     |
| R6 class             | `expect_r6_class(obj, "MyR6Class")`            |
| Shape (matrix/array) | `expect_shape(matrix, c(10, 5))`               |
| TRUE/FALSE value     | `expect_true(expr)` / `expect_false(expr)`     |
| All elements TRUE    | `expect_all_true(vector > 0)`                  |
| All elements FALSE   | `expect_all_false(vector < 0)`                 |
| Set equality         | `expect_setequal(x, y)`                        |
| Contains element     | `expect_contains(fruits, "apple")`             |
| Element in set       | `expect_in("apple", fruits)`                   |
| Disjoint sets        | `expect_disjoint(set1, set2)`                  |

### Numeric Comparisons

Use `expect_equal()` — it has a tolerance by default. For exact integer/numeric checks use `expect_identical()` only when you really mean bit-exact equality:

```r
# Preferred: checks numeric value with tolerance (good for floats)
expect_equal(as.numeric(result), 1609459200)

# Use only for exact bit-by-bit equality
expect_identical(10L, 10L)

# Allows custom tolerance
expect_equal(10, 10 + 1e-7)

# Check all elements match (v3.3.0+)
expect_all_equal(x, expected)
```

## Design Principles

### 1. Self-Sufficient Tests

Each test should contain all setup, execution, and teardown code:

```r
# Good: self-contained
test_that("foofy() works", {
  data <- data.frame(x = 1:3, y = letters[1:3])
  result <- foofy(data)
  expect_equal(result$x, 1:3)
})

# Bad: relies on ambient state
dat <- data.frame(x = 1:3, y = letters[1:3])
test_that("foofy() works", {
  result <- foofy(dat)  # Where did 'dat' come from?
  expect_equal(result$x, 1:3)
})
```

### 2. Self-Contained Tests (Cleanup Side Effects)

Use `withr` to manage state changes:

```r
test_that("function respects options", {
  withr::local_options(my_option = "test_value")
  withr::local_envvar(MY_VAR = "test")
  withr::local_package("jsonlite")

  result <- my_function()
  expect_equal(result$setting, "test_value")
  # Automatic cleanup after test
})
```

**Common withr functions:**

- `local_options()` - Temporarily set options
- `local_envvar()` - Temporarily set environment variables
- `local_tempfile()` - Create temp file with automatic cleanup
- `local_tempdir()` - Create temp directory with automatic cleanup
- `local_package()` - Temporarily attach package

**Important:** Do **not** set a system timezone globally with `Sys.setenv(TZ = ...)`. Use `attr(result, "tzone")` locally instead (see examples in the "Common Patterns and Examples" section).

### 3. Plan for Test Failure

Write tests assuming they will fail and need debugging:

- Tests should run independently in fresh R sessions
- Avoid hidden dependencies on earlier tests
- Make test logic explicit and obvious

### 4. Repetition is Acceptable

Repeat setup code in tests rather than factoring it out. Test clarity is more important than avoiding duplication.

### 5. Use `devtools::load_all()` Workflow

During development:

- Use `devtools::load_all()` instead of `library()`
- Makes all functions available (including unexported)
- Automatically attaches testthat
- Eliminates need for `library()` calls in tests

## Common Patterns and Examples

### Testing POSIXct Timestamps

```r
expect_s3_class(result, "POSIXct")
expect_false(is.na(result))
expect_equal(as.numeric(result), 1609459200)

# check date components
expect_equal(format(result, "%Y"), "2023")
expect_equal(format(result, "%m"), "12")
expect_equal(format(result, "%d"), "10")
expect_equal(format(result, "%H"), "08")  # note: after tz conversion to UTC
```

### Normalizing Timezone for Assertions

```r
result <- my_function(timestamp)
attr(result, "tzone") <- "UTC"   # pin tz before asserting components
expect_equal(format(result, "%H"), "08")
```

### Testing Vectorized Functions

```r
mixed <- c("1609459200", "2021-01-01T12:30:45Z", "2021-01-01")
result <- parse_timestamps(mixed)
expect_length(result, 3)
expect_true(all(!is.na(result)))
```

### Testing Edge Cases

Always include tests for:

- Empty input (`character(0)`, `integer(0)`)
- `NA` values in a vector
- Invalid / unsupported formats (expect `NA` back, or an error — document the choice)
- Boundary values (e.g. millisecond vs. second threshold `1e11`)

```r
test_that("my_fn - empty vector - returns empty result", {
  result <- mypkg:::my_fn(character(0))
  expect_length(result, 0)
  expect_s3_class(result, "POSIXct")
})

test_that("my_fn - na values - preserved in result", {
  result <- mypkg:::my_fn(c("2021-01-01", NA, "2022-01-01"))
  expect_true(is.na(result[2]))
  expect_false(is.na(result[1]))
})
```

### Testing Multiple Related Cases

```r
test_that("str_trunc() - all directions", {
  trunc <- function(direction) {
    str_trunc("This string is moderately long", direction, width = 20)
  }

  expect_equal(trunc("right"), "This string is mo...")
  expect_equal(trunc("left"), "...erately long")
  expect_equal(trunc("center"), "This stri...ely long")
})
```

### Errors with Specific Classes

```r
test_that("validation - catches errors - with correct class", {
  expect_error(
    validate_input("wrong_type"),
    class = "vctrs_error_cast"
  )
})
```

### Testing with Temporary Files

```r
test_that("file processing - valid file - processes correctly", {
  temp_file <- withr::local_tempfile(
    lines = c("line1", "line2", "line3")
  )

  result <- process_file(temp_file)
  expect_equal(length(result), 3)
})
```

### Custom Expectations in Helper Files

```r
# In tests/testthat/helper-expectations.R
expect_valid_user <- function(user) {
  expect_type(user, "list")
  expect_named(user, c("id", "name", "email"))
  expect_type(user$id, "integer")
  expect_match(user$email, "@")
}

# In test file
test_that("user creation - valid email - creates user", {
  user <- create_user("test@example.com")
  expect_valid_user(user)
})
```

## Snapshot Testing

For complex output that's difficult to verify programmatically, use snapshot tests. See [references/snapshots.md](references/snapshots.md) for complete guide.

**Basic pattern:**

```r
test_that("error message is helpful", {
  expect_snapshot(
    error = TRUE,
    validate_input(NULL)
  )
})
```

Snapshots stored in `tests/testthat/_snaps/`.

**Workflow:**

```r
devtools::test()                    # Creates new snapshots
testthat::snapshot_review('name')   # Review changes
testthat::snapshot_accept('name')   # Accept changes
```

## Test Fixtures and Data

Three approaches for test data:

**1. Constructor functions** - Create data on-demand:

```r
new_sample_data <- function(n = 10) {
  data.frame(id = seq_len(n), value = rnorm(n))
}
```

**2. Local functions with cleanup** - Handle side effects:

```r
local_temp_csv <- function(data, env = parent.frame()) {
  path <- withr::local_tempfile(fileext = ".csv", .local_envir = env)
  write.csv(data, path, row.names = FALSE)
  path
}
```

**3. Static fixture files** - Store in `fixtures/` directory:

```r
data <- readRDS(test_path("fixtures", "sample_data.rds"))
```

See [references/fixtures.md](references/fixtures.md) for detailed fixture patterns.

## Mocking

Replace external dependencies during testing using `local_mocked_bindings()`. See [references/mocking.md](references/mocking.md) for comprehensive mocking strategies.

**Basic pattern:**

```r
test_that("function works with mocked dependency", {
  local_mocked_bindings(
    external_api = function(...) list(status = "success", data = "mocked")
  )

  result <- my_function_that_calls_api()
  expect_equal(result$status, "success")
})
```

## File System Discipline

**Always write to temp directory:**

```r
# Good
output <- withr::local_tempfile(fileext = ".csv")
write.csv(data, output)

# Bad - writes to package directory
write.csv(data, "output.csv")
```

**Access test fixtures with `test_path()`:**

```r
# Good - works in all contexts
data <- readRDS(test_path("fixtures", "data.rds"))

# Bad - relative paths break
data <- readRDS("fixtures/data.rds")
```

## Advanced Topics

For advanced testing scenarios, see:

- **[references/bdd.md](references/bdd.md)** - BDD-style testing with describe/it, nested specifications, test-first workflows
- **[references/snapshots.md](references/snapshots.md)** - Snapshot testing, transforms, variants
- **[references/mocking.md](references/mocking.md)** - Mocking strategies, webfakes, httptest2
- **[references/fixtures.md](references/fixtures.md)** - Fixture patterns, database fixtures, helper files
- **[references/advanced.md](references/advanced.md)** - Skipping tests, secrets management, CRAN requirements, custom expectations, parallel testing

## testthat 3 Modernizations

When working with testthat 3 code, prefer modern patterns:

**Deprecated → Modern:**

- `context()` → Remove (duplicates filename)
- `expect_equivalent()` → `expect_equal(ignore_attr = TRUE)`
- `with_mock()` → `local_mocked_bindings()`
- `is_null()`, `is_true()`, `is_false()` → `expect_null()`, `expect_true()`, `expect_false()`

**New in testthat 3:**

- Edition system (`Config/testthat/edition: 3`)
- Improved snapshot testing
- `waldo::compare()` for better diff output
- Unified condition handling
- `local_mocked_bindings()` works with byte-compiled code
- Parallel test execution support

## Quick Reference

**Initialize:** `usethis::use_testthat(3)`

**Run tests:** `devtools::test()` or Ctrl/Cmd + Shift + T

**Create test file:** `usethis::use_test("name")`

**Review snapshots:** `testthat::snapshot_review()`

**Accept snapshots:** `testthat::snapshot_accept()`

**Find slow tests:** `devtools::test(reporter = "slow")`

**Shuffle tests:** `devtools::test(shuffle = TRUE)`

## What Not to Do

- Do **not** use `source()` to load source files — use the package with `library()` or `devtools::load_all()`
- Do **not** put multiple unrelated behaviours in one `test_that()` block
- Do **not** set a system timezone globally with `Sys.setenv(TZ = ...)` — use `attr(result, "tzone")` locally instead
- Do **not** use `print()` or `cat()` for debugging in committed tests
- Do **not** write tests that depend on the current system time unless unavoidable; prefer fixed timestamps
- Do **not** call `library()` inside individual `test_that()` blocks — load packages once at the top of the test file
- Do **not** add `@export` to functions just to make them testable — use `:::` for internal functions
- Do **not** write tests that rely on hidden dependencies on earlier tests — each test should be independent
