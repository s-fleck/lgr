# Changelog

## lgr (development version)

### Added

- **Docs:** Add a SKILL.md documenting how to use lgr in R packages, and
  [`use_skill()`](https://s-fleck.github.io/lgr/reference/use_skill.md)
  to install it into a project’s coding-agent config directory
  (auto-detecting `.claude/`, `.cursor/`, `.codex/`, and other supported
  presets, or accepting an explicit `agent`/`path`)

- **Docs:** Complete rewrite of the code documentation and update ro
  roxygen2 8.0

- **Docs:** Refresh the pkgdown site: update the CI workflow to match
  the current r-lib/actions template (newer action versions, SHA-pinned
  deploy action), regenerate favicons, review the reference index for
  completeness, and add per-pull-request preview site deployments with
  an auto-updating PR comment linking to the preview

### Changed

- **Breaking:** Complete overhaul of the logging config file system.
  Replace the undocumented `Logger:` shorthand and hidden multi-logger
  `logging_config` format with a single, documented, exported
  declarative configuration format (`version: 1`). Drop the top-level
  `Logger:` key and put settings under `loggers: {root: {...}}`; replace
  implicit `ClassName:` keys with an explicit `class:` key
  (e.g. `AppenderConsole: {threshold: info}` becomes
  `{class: AppenderConsole, threshold: info}`). A plain `list` of
  already-constructed objects passed to `Logger$config()` continues to
  work unchanged for a single logger. See
  [`vignette("lgr", package = "lgr")`](https://s-fleck.github.io/lgr/articles/lgr.md)
  and [`?config`](https://s-fleck.github.io/lgr/reference/config.md) for
  the format reference and a migration guide

### Fixed

- **Appender:** Fix `$remove_appenders()` removing the wrong appenders
  when removing multiple indices at once (thx
  [@m-muecke](https://github.com/m-muecke))

- **AppenderFileRotating:** Fix `$format()` returning an invalid value
  instead of `FALSE` for the colour default (thx
  [@m-muecke](https://github.com/m-muecke))

- **Docs:** Fix a couple of documentation typos (thx
  [@m-muecke](https://github.com/m-muecke))

- **Filter:** Fix `$remove_filters()` removing the wrong filters when
  removing multiple indices at once (thx
  [@m-muecke](https://github.com/m-muecke))

- **Layout:** Fix an incorrect
  [`sprintf()`](https://rdrr.io/r/base/sprintf.html) call in the
  unknown-level warning of `LayoutFormat$read()` (thx
  [@m-muecke](https://github.com/m-muecke))

- **LayoutJson:** Fix `$set_transform_event_names()` not returning
  `self`, which broke method chaining (thx
  [@m-muecke](https://github.com/m-muecke))

- **Logger:** Fix
  [`add_log_levels()`](https://s-fleck.github.io/lgr/reference/get_log_levels.md)
  not showing the name of the overwritten level in its message (thx
  [@m-muecke](https://github.com/m-muecke))

- **Logger:** Fix
  [`basic_config()`](https://s-fleck.github.io/lgr/reference/basic_config.md)
  not validating the `memory` argument against itself (thx
  [@m-muecke](https://github.com/m-muecke))

- **logger_tree:** Fix subtle styling not being disabled when
  `color = FALSE` (thx [@m-muecke](https://github.com/m-muecke))

## lgr 0.5.2 (2026-01-30)

CRAN release: 2026-01-30

### Fixed

- **AppenderFileRotatingTime:** Fix the class being registered as
  `AppenderFileRotating` instead of `AppenderFileRotatingTime`, which
  broke R6 class dispatch

- **DESCRIPTION:** Quote ‘Dynatrace’ and ‘lgrExtra’ package names

## lgr 0.5.0 (2025-07-23)

CRAN release: 2025-07-23

### Added

- **Layout:** Add `excluded_fields` for excluding fields from logging

- **LayoutGlue:** Support transformers (see
  [`?glue::glue`](https://glue.tidyverse.org/reference/glue.html))
  ([\#51](https://github.com/s-fleck/lgr/issues/51))

- **LayoutJson:** Add `transform_event` and `transform_event_names`
  fields for more flexible event transformation; excludes `rawMsg` by
  default for backwards compatibility

- **LogEvent:** Add a `rawMsg` property to store the message before
  string interpolation (i.e. the original string with unresolved
  [`sprintf()`](https://rdrr.io/r/base/sprintf.html)/[`glue()`](https://glue.tidyverse.org/reference/glue.html)
  placeholders) ([\#60](https://github.com/s-fleck/lgr/issues/60))

### Changed

- **AppenderConsole:** Accept a `connection` argument; defaults to
  `stderr` instead of `stdout` when called from a **knitr** rendering
  process, to avoid polluting markdown documents
  ([\#62](https://github.com/s-fleck/lgr/issues/62), thx
  [@gadenbuie](https://github.com/gadenbuie))

- **Internals:** Use
  [`isTRUE()`](https://rdrr.io/r/base/Logic.html)/[`isFALSE()`](https://rdrr.io/r/base/Logic.html)
  instead of `identical(x, TRUE)` for better performance (thx
  [@m-muecke](https://github.com/m-muecke))

- **Internals:** Use [`anyNA()`](https://rdrr.io/r/base/NA.html) and
  [`anyDuplicated()`](https://rdrr.io/r/base/duplicated.html) in more
  places for better performance (thx
  [@m-muecke](https://github.com/m-muecke))

- **Logging:** Replace `NULL` values and empty strings with the string
  `"<NULL>"` instead of an empty log message
  ([\#51](https://github.com/s-fleck/lgr/issues/51))

## lgr 0.4.4 (2022-09-05)

CRAN release: 2022-09-05

### Added

- **LogEvent:** Add
  [`as_LogEvent()`](https://s-fleck.github.io/lgr/reference/as_LogEvent.md)
  to coerce event-like objects to `LogEvent`s

### Changed

- **Docs:** Use the more common term “structured logging” instead of
  “custom fields” where appropriate

- **Docs:** Rebuild documentation for compatibility with R 4.2.0

### Fixed

- **event_list:** Fix
  [`as_event_list.data.frame()`](https://s-fleck.github.io/lgr/reference/event_list.md)
  not returning a list of `LogEvent`s

- **format.LogEvent:** Fix `%k`/`%K` parameters not working as expected
  with **crayon** terminal colours
  ([\#52](https://github.com/s-fleck/lgr/issues/52))

- **LayoutGlue:** Fix the default format string using `msg` instead of
  `{msg}` ([\#54](https://github.com/s-fleck/lgr/issues/54), thx
  [@mmuurr](https://github.com/mmuurr))

## lgr 0.4.3 (2021-09-16)

CRAN release: 2021-09-16

### Added

- **logger_index:** Add
  [`logger_index()`](https://s-fleck.github.io/lgr/reference/logger_index.md),
  returning a `data.frame` with metadata on all registered loggers
  ([\#47](https://github.com/s-fleck/lgr/issues/47), thanks
  [@Fuco1](https://github.com/Fuco1))

- **string_repr:** Export the
  [`string_repr()`](https://s-fleck.github.io/lgr/reference/string_repr.md)
  generic used to format R objects for log message output
  ([\#48](https://github.com/s-fleck/lgr/issues/48), thanks
  [@mmuurr](https://github.com/mmuurr))

### Changed

- **Logger:** `$log()` now unpacks conditions passed to it (except when
  supplied as a named argument), for both `Logger` and `LoggerGlue`
  ([\#45](https://github.com/s-fleck/lgr/issues/45), thanks
  [@mmuurr](https://github.com/mmuurr))

### Fixed

- **Tests:** Fix timezone-related test failures on CRAN

## lgr 0.4.2 (2021-01-10)

CRAN release: 2021-01-10

### Changed

- **default_exception_handler:** Throw more informative warnings when an
  error is encountered during logging

### Deprecated

- **AppenderFileRotating:** Deprecate the `create_file` argument; file
  creation is now always enabled (hardcoded to `TRUE`, since `FALSE`
  never made sense)

### Fixed

- **Tests:** Drop tests for deprecated **future** plans to maintain
  compatibility with upcoming **future** versions
  ([\#43](https://github.com/s-fleck/lgr/issues/43))

## lgr 0.4.1 (2020-10-20)

CRAN release: 2020-10-20

### Added

- **AppenderFile:** `$show()` can now filter log files formatted by
  `LayoutFormat` by log level (note: this greps the file, so it may
  return false positives when a log message itself contains level-like
  strings)

- **AppenderMemory:** Add a `$clear()` method to clear the buffer
  without sending events to its attached appenders

- **LayoutJson:** Add a `timestamp_fmt` field for custom timestamp
  formats ([\#34](https://github.com/s-fleck/lgr/issues/34))

- **LogEvent:** Add a
  [`toString()`](https://rdrr.io/r/base/toString.html) method for a
  compact representation of `LogEvent`s

- **Logger:** Add a `$list_log()` method

- **Utils:** Export more utility functions useful for building custom
  Appenders, such as
  [`standardize_threshold()`](https://s-fleck.github.io/lgr/reference/standardize_threshold.md)
  and
  [`event_list()`](https://s-fleck.github.io/lgr/reference/event_list.md)

### Changed

- **AppenderBuffer:** Default `flush_threshold` to `NULL` (never flush
  based on the log level of an event); `flush_threshold` is now
  independent of `should_flush`

- **AppenderFile:** `$show()` and `$data` now dispatch to
  `Layout$read()`/`Layout$parse()`, tying log file reading/parsing to
  the Layout used

- **AppenderFileRotating:** Update `AppenderFileRotating` and related
  appenders for compatibility with
  [rotor](https://github.com/s-fleck/rotor) 0.3.0

- **Docs:** Rewrite documentation to use the newer roxygen2 features for
  R6 classes

- **Errors:** Give most errors appropriate subclasses

- **Tests:** lgr is now automatically tested against all major R
  versions \>= 3.2.0

### Removed

- **Breaking:** Move the more complex Appenders to the new
  [lgrExtra](https://github.com/s-fleck/lgrExtra) package, including
  database Appenders, email and push notifications, and `AppenderDt`
  (in-memory `data.table`)

- **AppenderBuffer:** Remove `default_should_flush()`, made unnecessary
  now that `flush_threshold` is independent of `should_flush`

### Fixed

- **basic_config:** Fix `.jsonl` file handling to work as documented

- **Logger:** `$log()` now dispatches to all appenders even if one
  throws an error, instead of aborting after the first failing appender

## lgr 0.3.4 (2020-03-20)

CRAN release: 2020-03-20

### Fixed

- Fix compatibility with R \< 3.6.0
  ([\#32](https://github.com/s-fleck/lgr/issues/32))

## lgr 0.3.3 (2019-09-27)

CRAN release: 2019-09-27

### Fixed

- **Logger:** Fix a performance regression in parent lookup that notably
  slowed down “do-nothing” logging (i.e. discarding a message below a
  logger’s threshold)

## lgr 0.3.2 (2019-08-20)

CRAN release: 2019-08-20

### Added

- **AppenderSyslog:** Add `AppenderSyslog` for logging to syslog via
  [rsyslog](https://github.com/atheriel/rsyslog) (thanks
  [@atheriel](https://github.com/atheriel))

## lgr 0.3.1 (2019-06-13)

CRAN release: 2019-06-13

### Added

- **Appender:** Add
  [`print()`](https://rdrr.io/r/base/print.html)/[`format()`](https://rdrr.io/r/base/format.html)
  methods

- **AppenderMemory:** Add `data`/`dt` active fields to return the log as
  a `data.frame`/`data.table`

- **logger_tree:** Add
  [`logger_tree()`](https://s-fleck.github.io/lgr/reference/logger_tree.md)
  for an overview of all registered loggers

### Removed

- **Breaking:** Remove the deprecated `FATAL()`/`ERROR()` functions; use
  `lgr$fatal()`/`lgr$error()` instead

### Fixed

- **AppenderMemory:** Fix `$buffer_dt()`/`$show()` not handling custom
  fields containing atomic vectors correctly

## lgr 0.3.0 (2019-05-30)

CRAN release: 2019-05-30

### Added

- **AppenderFileRotating:** Add support for rotating log files via
  `AppenderFileRotating`, `AppenderFileRotatingDate`, and
  `AppenderFileRotatingTime` (requires the
  [rotor](https://github.com/s-fleck/rotor) package)

- **basic_config:**
  [`basic_config()`](https://s-fleck.github.io/lgr/reference/basic_config.md)
  now accepts thresholds (e.g. `"info"`, `"fatal"`) as arguments to
  `console` and `memory`

- **Config:** `$config()` now accepts YAML in addition to JSON, as files
  or as a character string

- **Config:** `$config(NULL)` resets a Logger to its
  default/unconfigured state

- **get_logger:** Add a `reset` argument to
  [`get_logger()`](https://s-fleck.github.io/lgr/reference/get_logger.md)
  that completely resets a Logger’s configuration and replaces special
  loggers (such as `LoggerGlue`) with vanilla ones

- **Root logger:** The root logger can now be configured via
  [`options()`](https://rdrr.io/r/base/options.html) and/or environment
  variables (see
  [`?lgr`](https://s-fleck.github.io/lgr/reference/lgr-package.md))

### Changed

- **AppenderFile:** `$new()` now creates an empty file immediately, or
  fails if it can’t

- **Breaking:** Change the default config of the root logger to have
  only a console appender with a default threshold of `"info"`
  (previously `"all"`). To restore the old behaviour, run
  `basic_config(threshold = "all", console = "info", memory = "all")`

- **RMariaDB:** Improve support for RMariaDB and drop support for RMySQL

- **RPostgres:** Improve support for RPostgres and drop support for
  RPostgreSQL

- **show_log:**
  [`show_log()`](https://s-fleck.github.io/lgr/reference/simple_logging.md),
  [`show_data()`](https://s-fleck.github.io/lgr/reference/simple_logging.md),
  and friends now accept logger names in addition to `Logger`/`Appender`
  objects as `target`

- **with_log_level:**
  [`with_log_level()`](https://s-fleck.github.io/lgr/reference/with_log_level.md)
  and
  [`with_log_value()`](https://s-fleck.github.io/lgr/reference/with_log_level.md)
  now accept logger names in addition to `Logger` objects as the
  `logger` argument

### Deprecated

- Deprecate `FATAL()`/`ERROR()`; use `lgr$fatal()`/`lgr$error()` instead

### Fixed

- **get_logger_glue:** Fix
  [`get_logger_glue()`](https://s-fleck.github.io/lgr/reference/get_logger.md)
  not working as intended

## lgr 0.2.1 (2019-03-25)

CRAN release: 2019-03-25

### Removed

- **Docs:** Remove the `.Rd` file for the unexported `LoggerRoot` class

### Fixed

- **Tests:** Fix the test suite not cleaning up temporary files

## lgr 0.2.0 (2019-03-22)

CRAN release: 2019-03-22

### Added

- **Logger:** Add an experimental `$config()` method for configuring
  Loggers via config objects and YAML files

- **Logger:** New Loggers without an explicit threshold now inherit the
  threshold of their parent

- **with_logging:** Add
  [`with_logging()`](https://s-fleck.github.io/lgr/reference/suspend_logging.md),
  the opposite of
  [`without_logging()`](https://s-fleck.github.io/lgr/reference/suspend_logging.md),
  for toggling logging on/off within specific unit tests

### Changed

- **Breaking:** Derive Logger inheritance from a Logger’s qualified
  name; `lg$parent` is now derived from `lg$name` and `lg$set_parent()`
  is no longer possible

- **Breaking:** Remove the `full_name` active binding from Loggers;
  `name` is now always the fully qualified name (what `full_name` used
  to return); the `ancestry` format method was revised accordingly

- **Dependencies:** Depend on R6 \>= 2.4.0, which includes relevant
  finalizer fixes; `finalize` methods are now private

- **get_loggers:** `get_loggers()` now registers new loggers in the
  `lgr::loggers` namespace, a more global and decoupled approach similar
  to Python’s `logging` module

## lgr 0.1.1 (2019-02-22)

CRAN release: 2019-02-22

### Added

- **show_data:** Add
  [`show_data()`](https://s-fleck.github.io/lgr/reference/simple_logging.md)/[`show_dt()`](https://s-fleck.github.io/lgr/reference/simple_logging.md)
  for quick access to the root logger’s in-memory log as a
  `data.frame`/`data.table` respectively

### Fixed

- **Docs:** Remove non-breaking spaces from `.Rd` files, which caused
  problems compiling the PDF manual during CRAN submission

- Numerous small fixes
