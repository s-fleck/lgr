# Read and validate an lgr configuration

Parses, interpolates and validates a declarative lgr configuration
without applying it to any logger. Useful for checking a config file
(e.g. in CI) or for programmatically inspecting a config before applying
it. See [config](https://s-fleck.github.io/lgr/reference/config.md) for
the format.

## Usage

``` r
read_config(x, allow_code = FALSE)

validate_config(x, allow_code = FALSE)
```

## Arguments

- x:

  a `character` scalar (a file path, or a YAML/JSON string), or a `list`
  following the config format described in
  [config](https://s-fleck.github.io/lgr/reference/config.md).

- allow_code:

  `TRUE` or `FALSE`. Whether to permit the inline `code:` form for
  `exception_handler` and filter entries, which evaluates
  config-supplied R source. Defaults to `FALSE` (safe mode).

## Value

an object of class `lgr_config` (a validated, interpolated nested
`list`), invisibly usable with
[Logger\$config()](https://s-fleck.github.io/lgr/reference/Logger.md).

`validate_config()` returns `TRUE` (invisibly) if `x` is valid, and
throws an informative error otherwise.

## Examples

``` r
cfg <- read_config(list(loggers = list(root = list(threshold = "warn"))))
validate_config(list(loggers = list(root = list(threshold = "warn"))))
```
