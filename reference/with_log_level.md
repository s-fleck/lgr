# Inject Values into Logging Calls

`with_log_level` temporarily overrides the log level of all
[LogEvents](https://s-fleck.github.io/lgr/reference/LogEvent.md) created
by target [Logger](https://s-fleck.github.io/lgr/reference/Logger.md).

## Usage

``` r
with_log_level(level, code, logger = lgr::lgr)

with_log_value(values, code, logger = lgr::lgr)
```

## Arguments

- level:

  `integer` or `character` scalar: the desired log level

- code:

  Any R code

- logger:

  a [Logger](https://s-fleck.github.io/lgr/reference/Logger.md) or the
  name of one (see
  [`get_logger()`](https://s-fleck.github.io/lgr/reference/get_logger.md)).
  Defaults to the root logger
  ([`lgr::lgr`](https://s-fleck.github.io/lgr/reference/lgr-package.md)).

- values:

  a named `list` of values to be injected into the logging calls

## Value

whatever `code` would return

## Details

These functions abuses lgr's filter mechanic to modify LogEvents
in-place before they passed on the Appenders. Use with care as they can
produce hard to reason about code.

## Examples

``` r
with_log_level("warn", {
  lgr$info("More important than it seems")
  lgr$fatal("Really not so bad")
})
with_log_value(
  list(msg = "overriden msg"),  {
  lgr$info("bar")
  lgr$fatal("FOO")
})
```
