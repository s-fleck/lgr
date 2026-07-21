# LoggerGlue

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

[`Filterable`](https://s-fleck.github.io/lgr/reference/Filterable.md)
-\> [`Logger`](https://s-fleck.github.io/lgr/reference/Logger.md) -\>
`LoggerGlue`

## Active bindings

- `transformer`:

  `NULL` or a `function`. The `.transformer` passed to
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html) for
  string interpolation. See `$set_transformer()`.

## Methods

### Public methods

- [`LoggerGlue$new()`](#method-LoggerGlue-initialize)

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

- [`Filterable$add_filter()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-add_filter)
- [`Filterable$filter()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-filter)
- [`Filterable$remove_filter()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-remove_filter)
- [`Filterable$set_filters()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-set_filters)
- [`Logger$add_appender()`](https://s-fleck.github.io/lgr/reference/Logger.html#method-add_appender)
- [`Logger$config()`](https://s-fleck.github.io/lgr/reference/Logger.html#method-config)
- [`Logger$handle_exception()`](https://s-fleck.github.io/lgr/reference/Logger.html#method-handle_exception)
- [`Logger$remove_appender()`](https://s-fleck.github.io/lgr/reference/Logger.html#method-remove_appender)
- [`Logger$set_appenders()`](https://s-fleck.github.io/lgr/reference/Logger.html#method-set_appenders)
- [`Logger$set_exception_handler()`](https://s-fleck.github.io/lgr/reference/Logger.html#method-set_exception_handler)
- [`Logger$set_propagate()`](https://s-fleck.github.io/lgr/reference/Logger.html#method-set_propagate)
- [`Logger$set_replace_empty()`](https://s-fleck.github.io/lgr/reference/Logger.html#method-set_replace_empty)
- [`Logger$set_threshold()`](https://s-fleck.github.io/lgr/reference/Logger.html#method-set_threshold)

------------------------------------------------------------------------

### `LoggerGlue$new()`

Create a new `LoggerGlue`. As with
[Logger\$new()](https://s-fleck.github.io/lgr/reference/Logger.md), you
should not normally call this directly but use
[`get_logger_glue()`](https://s-fleck.github.io/lgr/reference/get_logger.md)
instead.

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

#### Arguments

- `name, appenders, threshold, filters, exception_handler, propagate`:

  See section Active bindings.

- `replace_empty`:

  The replacement value used for empty log message arguments (`NULL` or
  empty vectors). See `$set_replace_empty()`.

- `transformer`:

  `NULL` or a `function` with two arguments (`text`, `envir`) that is
  passed on to
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html) as
  its `.transformer` argument to customize string interpolation. See
  `$set_transformer()`.

------------------------------------------------------------------------

### `LoggerGlue$fatal()`

Log an Event fatal priority using
[`glue::glue()`](https://glue.tidyverse.org/reference/glue.html) for
string interpolation.

#### Usage

    LoggerGlue$fatal(..., caller = get_caller(-8L), .envir = parent.frame())

#### Arguments

- `...`:

  unnamed `character` fragments are glued together with
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html) to
  construct the log message; named arguments are turned into custom
  fields (a leading `.` in the name suppresses this). See the class
  description for details.

- `.envir`:

  the environment in which `glue()` evaluates `{expr}` placeholders. See
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html).

- `msg, ..., caller`:

  see `$log()`

------------------------------------------------------------------------

### `LoggerGlue$error()`

Log an Event error priority using
[`glue::glue()`](https://glue.tidyverse.org/reference/glue.html) for
string interpolation.

#### Usage

    LoggerGlue$error(..., caller = get_caller(-8L), .envir = parent.frame())

#### Arguments

- `...`:

  unnamed `character` fragments are glued together with
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html) to
  construct the log message; named arguments are turned into custom
  fields (a leading `.` in the name suppresses this). See the class
  description for details.

- `.envir`:

  the environment in which `glue()` evaluates `{expr}` placeholders. See
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html).

- `msg, ..., caller`:

  see `$log()`

------------------------------------------------------------------------

### `LoggerGlue$warn()`

Log an Event warn priority using
[`glue::glue()`](https://glue.tidyverse.org/reference/glue.html) for
string interpolation.

#### Usage

    LoggerGlue$warn(..., caller = get_caller(-8L), .envir = parent.frame())

#### Arguments

- `...`:

  unnamed `character` fragments are glued together with
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html) to
  construct the log message; named arguments are turned into custom
  fields (a leading `.` in the name suppresses this). See the class
  description for details.

- `.envir`:

  the environment in which `glue()` evaluates `{expr}` placeholders. See
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html).

- `msg, ..., caller`:

  see `$log()`

------------------------------------------------------------------------

### `LoggerGlue$info()`

Log an Event info priority using
[`glue::glue()`](https://glue.tidyverse.org/reference/glue.html) for
string interpolation.

#### Usage

    LoggerGlue$info(..., caller = get_caller(-8L), .envir = parent.frame())

#### Arguments

- `...`:

  unnamed `character` fragments are glued together with
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html) to
  construct the log message; named arguments are turned into custom
  fields (a leading `.` in the name suppresses this). See the class
  description for details.

- `.envir`:

  the environment in which `glue()` evaluates `{expr}` placeholders. See
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html).

- `msg, ..., caller`:

  see `$log()`

------------------------------------------------------------------------

### `LoggerGlue$debug()`

Log an Event debug priority using
[`glue::glue()`](https://glue.tidyverse.org/reference/glue.html) for
string interpolation.

#### Usage

    LoggerGlue$debug(..., caller = get_caller(-8L), .envir = parent.frame())

#### Arguments

- `...`:

  unnamed `character` fragments are glued together with
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html) to
  construct the log message; named arguments are turned into custom
  fields (a leading `.` in the name suppresses this). See the class
  description for details.

- `.envir`:

  the environment in which `glue()` evaluates `{expr}` placeholders. See
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html).

- `msg, ..., caller`:

  see `$log()`

------------------------------------------------------------------------

### `LoggerGlue$trace()`

Log an Event trace priority using
[`glue::glue()`](https://glue.tidyverse.org/reference/glue.html) for
string interpolation.

#### Usage

    LoggerGlue$trace(..., caller = get_caller(-8L), .envir = parent.frame())

#### Arguments

- `...`:

  unnamed `character` fragments are glued together with
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html) to
  construct the log message; named arguments are turned into custom
  fields (a leading `.` in the name suppresses this). See the class
  description for details.

- `.envir`:

  the environment in which `glue()` evaluates `{expr}` placeholders. See
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html).

- `msg, ..., caller`:

  see `$log()`

------------------------------------------------------------------------

### `LoggerGlue$log()`

Log an event, constructing the message with
[`glue::glue()`](https://glue.tidyverse.org/reference/glue.html) instead
of [`sprintf()`](https://rdrr.io/r/base/sprintf.html). Unlike
`Logger$log()`, there is no dedicated `msg` argument: unnamed fragments
in `...` are glued together to form the message, and named arguments are
available to `glue()` as temporary variables (and become custom fields
unless their name starts with `.`).

#### Usage

    LoggerGlue$log(
      level,
      ...,
      timestamp = Sys.time(),
      caller = get_caller(-7),
      .envir = parent.frame()
    )

#### Arguments

- `level`:

  a `character` or `integer` scalar. See
  [log_levels](https://s-fleck.github.io/lgr/reference/get_log_levels.md).

- `...`:

  unnamed `character` fragments are glued together with
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html) to
  construct the log message; named arguments are turned into custom
  fields (a leading `.` in the name suppresses this).

- `timestamp`:

  [POSIXct](https://rdrr.io/r/base/DateTimeClasses.html). Timestamp of
  the event.

- `caller`:

  a `character` scalar. The name of the calling function.

- `.envir`:

  the environment in which `glue()` evaluates `{expr}` placeholders. See
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html).

------------------------------------------------------------------------

### `LoggerGlue$list_log()`

`list_log()` is a shortcut for `do.call(LoggerGlue$log, x)`. Unlike
`Logger$list_log()`, an element named `msg` in `x` is treated as an
unnamed `glue()` fragment rather than a dedicated message argument.

#### Usage

    LoggerGlue$list_log(x)

#### Arguments

- `x`:

  a named `list` that must at least contain the named elements `level`
  and `timestamp`

------------------------------------------------------------------------

### `LoggerGlue$spawn()`

Spawn a child `LoggerGlue`. Equivalent to
`get_logger_glue(paste0(name, "/", child))`.

#### Usage

    LoggerGlue$spawn(name)

#### Arguments

- `name`:

  `character` vector. Name of the child logger
  `get_logger("foo/bar")$spawn("baz")` is equivalent to
  `get_logger("foo/bar/baz")`

------------------------------------------------------------------------

### `LoggerGlue$set_transformer()`

Set the transformer for glue string interpolation

#### Usage

    LoggerGlue$set_transformer(x)

#### Arguments

- `x`:

  single [function](https://rdrr.io/r/base/function.html) taking two
  arguments. See
  [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html).
