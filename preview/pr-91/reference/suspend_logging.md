# Suspend All Logging

Completely disable logging for all loggers. This is for example useful
for automated test code. `suspend_logging()` globally disables all
logging with lgr until `unsuspend_logging()` is invoked, while
`without_logging()` and `with_logging()` temporarily disable/enable
logging.

## Usage

``` r
suspend_logging()

unsuspend_logging()

without_logging(code)

with_logging(code)
```

## Arguments

- code:

  Any R code

## Value

`suspend_logging()` and `unsuspend_logging()` return `NULL` (invisibly),
`without_logging()` and `with_logging()` returns whatever `code`
returns.

## Examples

``` r
lg <- get_logger("test")

# temporarily disable logging
lg$fatal("foo")
without_logging({
  lg$info("everything in this codeblock will be suppressed")
  lg$fatal("bar")
})

# globally disable logging
suspend_logging()
lg$fatal("bar")
with_logging(lg$fatal("foo"))  # log anyways

# globally enable logging again
unsuspend_logging()
lg$fatal("foo")
```
