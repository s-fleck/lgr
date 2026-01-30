# LoggerGlue

LoggerGlue

LoggerGlue

## Details

`LoggerGlue` uses
[`glue::glue()`](https://glue.tidyverse.org/reference/glue.html) instead
of [`base::sprintf()`](https://rdrr.io/r/base/sprintf.html) to construct
log messages. **glue** is a very well designed package for string
interpolation. It makes composing log messages more flexible and
comfortable at the price of an additional dependency and slightly less
performance than [`sprintf()`](https://rdrr.io/r/base/sprintf.html).

`glue()` lets you define temporary named variables inside the call. As
with the normal Logger, these named arguments get turned into custom
fields; however, you can suppress this behaviour by making named
argument start with a `"."`. Please refer to
[`vignette("lgr", package = "lgr")`](https://s-fleck.github.io/lgr/articles/lgr.md)
for examples.

## Super classes

[`lgr::Filterable`](https://s-fleck.github.io/lgr/reference/Filterable.md)
-\> [`lgr::Logger`](https://s-fleck.github.io/lgr/reference/Logger.md)
-\> `LoggerGlue`

## Methods

### Public methods

- [`LoggerGlue$new()`](#method-LoggerGlue-new)

- [`LoggerGlue$fatal()`](#method-LoggerGlue-fatal)

- [`LoggerGlue$error()`](#method-LoggerGlue-error)

- [`LoggerGlue$warn()`](#method-LoggerGlue-warn)

- [`LoggerGlue$info()`](#method-LoggerGlue-info)

- [`LoggerGlue$debug()`](#method-LoggerGlue-debug)

- [`LoggerGlue$trace()`](#method-LoggerGlue-trace)

- [`LoggerGlue$log()`](#method-LoggerGlue-log)

- [`LoggerGlue$list_log()`](#method-LoggerGlue-list_log)

- [`LoggerGlue$spawn()`](#method-LoggerGlue-spawn)

- [`LoggerGlue$set_transformer()`](#method-LoggerGlue-set_transformer)

Inherited methods

- [`lgr::Filterable$add_filter()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-add_filter)
- [`lgr::Filterable$filter()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-filter)
- [`lgr::Filterable$remove_filter()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-remove_filter)
- [`lgr::Filterable$set_filters()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-set_filters)
- [`lgr::Logger$add_appender()`](https://s-fleck.github.io/lgr/reference/Logger.html#method-add_appender)
- [`lgr::Logger$config()`](https://s-fleck.github.io/lgr/reference/Logger.html#method-config)
- [`lgr::Logger$handle_exception()`](https://s-fleck.github.io/lgr/reference/Logger.html#method-handle_exception)
- [`lgr::Logger$remove_appender()`](https://s-fleck.github.io/lgr/reference/Logger.html#method-remove_appender)
- [`lgr::Logger$set_appenders()`](https://s-fleck.github.io/lgr/reference/Logger.html#method-set_appenders)
- [`lgr::Logger$set_exception_handler()`](https://s-fleck.github.io/lgr/reference/Logger.html#method-set_exception_handler)
- [`lgr::Logger$set_propagate()`](https://s-fleck.github.io/lgr/reference/Logger.html#method-set_propagate)
- [`lgr::Logger$set_replace_empty()`](https://s-fleck.github.io/lgr/reference/Logger.html#method-set_replace_empty)
- [`lgr::Logger$set_threshold()`](https://s-fleck.github.io/lgr/reference/Logger.html#method-set_threshold)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    LoggerGlue$new(
      name = "(unnamed logger)",
      appenders = list(),
      threshold = NULL,
      filters = list(),
      exception_handler = default_exception_handler,
      propagate = TRUE,
      replace_empty = "<NULL>",
      transformer = NULL
    )

------------------------------------------------------------------------

### Method `fatal()`

#### Usage

    LoggerGlue$fatal(..., caller = get_caller(-8L), .envir = parent.frame())

------------------------------------------------------------------------

### Method `error()`

#### Usage

    LoggerGlue$error(..., caller = get_caller(-8L), .envir = parent.frame())

------------------------------------------------------------------------

### Method `warn()`

#### Usage

    LoggerGlue$warn(..., caller = get_caller(-8L), .envir = parent.frame())

------------------------------------------------------------------------

### Method `info()`

#### Usage

    LoggerGlue$info(..., caller = get_caller(-8L), .envir = parent.frame())

------------------------------------------------------------------------

### Method [`debug()`](https://rdrr.io/r/base/debug.html)

#### Usage

    LoggerGlue$debug(..., caller = get_caller(-8L), .envir = parent.frame())

------------------------------------------------------------------------

### Method [`trace()`](https://rdrr.io/r/base/trace.html)

#### Usage

    LoggerGlue$trace(..., caller = get_caller(-8L), .envir = parent.frame())

------------------------------------------------------------------------

### Method [`log()`](https://rdrr.io/r/base/Log.html)

#### Usage

    LoggerGlue$log(
      level,
      ...,
      timestamp = Sys.time(),
      caller = get_caller(-7),
      .envir = parent.frame()
    )

------------------------------------------------------------------------

### Method `list_log()`

#### Usage

    LoggerGlue$list_log(x)

------------------------------------------------------------------------

### Method `spawn()`

#### Usage

    LoggerGlue$spawn(name)

------------------------------------------------------------------------

### Method `set_transformer()`

Set the transformer for glue string interpolation

#### Usage

    LoggerGlue$set_transformer(x)

#### Arguments

- `x`:

  single [function](https://rdrr.io/r/base/function.html) taking two
  arguments. See
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html).
