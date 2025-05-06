# lgr 0.4.5.9000

* `AppenderConsole` now accept a `connection` argument. If called from a 
  `{knitr}` rendering process, log messages are now output to `stderr` instead 
  of `stdout` by default, to avoid polluting markdown documents (#62, thx @gadenbuie).

* added hidden `.rawMsg` property to LogEvents to store message without
  string interpolation (e.g. that still contains the placeholders from 
  `sprintf()` or `glue()`) (#60)


# lgr 0.4.4

* `%k` and `%K` parameters in `format.LogEvent` now work as expected when using
  **crayon** terminal colours (#52).
  
* Fix default format string for `LayoutGlue` which was using `msg` 
  instead of `{msg}` (#54, thx @mmuurr)  
  
* Update docs to use the more common term "structured logging" instead of 
  "custom fields" where appropriate

* `as_event_list.data.frame` now really returns a list of `LogEvents`

* added `as_LogEvent()` to coerce various event-like objects to `LogEvents`

* rebuild docs for R 4.2.0


# lgr 0.4.3

* `logger_index()` returns a `data.frame` with metadata on all registered
  loggers (#47) (thanks @Fuco1)
  
* export new `string_repr()` generic that is used to layout R objects for 
  formatted log message output (#48, thanks @mmuurr)
  
* The `$log()` method of Logger and LoggerGlue now unpacks conditions (except
  if they are supplied as a named argument) (#45, thanks @mmuurr)
  
* Fix some timezone related tests for CRAN


# lgr 0.4.2

* Deprecated the `create_file` argument of `AppenderFileRotating*`. This
  is now hardcoded to `TRUE` (because `FALSE` doesn't really make sense here).

* `default_exception_handler()` now throws more informative warnings if an
  error is encountered during logging.

* drop tests for deprecated [future](https://cran.r-project.org/package=future) 
  plans to ensure compatibility with upcoming versions of future (#43)
  
  
# lgr 0.4.1

* Moved more complex Appenders to package 
  [lgrExtra](https://github.com/s-fleck/lgrExtra). This includes database 
  Appenders, email and push notifications and AppenderDt (in-memory 
  `data.tables`).
  
* `AppenderFile$show()` can now filter log files formatted by LayoutFormat 
  by log level. Be aware that this just `greps` through the file and therefore
  will return false positives on lines where the log message contains strings 
  that can be interpreted as log levels.
  
* `AppenderFile$show()` and `AppenderFile$data` now dispatches to 
  `Layout$read()` and `Layout$parse()`. This makes it possible to tie 
  reading/parsing of log files to Layouts.

* Loggers gain a `list_log()` method. See https://github.com/s-fleck/joblog  for 
  an R package that leverages this feature to create custom log event types for 
  tracking the status of cron jobs. 
  
* Export more utility functions that are useful for creating custom Appenders; 
  such as `standardize_threshold()` and `event_list()`.
  
* AppenderBuffer now defaults to `flush_threshold = NULL` 
  (never flush because of the log level of an event)
  
* `basic_config()` now works as documented for .jsonl files

* AppenderMemory gains a `$clear()` method that clears the buffer without 
  sending the events to it's attached appenders

* LayoutJson gains a `timestamp_fmt` field that can be used for custom 
  timestamp formats (#34)
  
* added `toString.LogEvent()` for compact representations of LogEvents

* lgr is now automatically tested for all major R version >= 3.2.0

* AppenderMemory/AppenderBuffer: `flush_threshold` is now independent of
  `should_flush` function. `default_should_flush()` is no longer necessary
  and has been removed.
  
* Updated AppenderFileRotating and co for compatibility with 
  [rotor](https://github.com/s-fleck/rotor) 0.3.0

* Most errors now have appropriate subclasses

* `Logger$log()` dispatches to all appenders - even if some throw an error -
  instead of aborting after the first Appender that throws an error
  
* complete rewrite of the documentation to use the new roxygen2 features for
  R6 classes.


# lgr 0.3.4

* Hotfix for compatibility with R < 3.6.0 (#32)


# lgr 0.3.3

* Fixed a performance regression when looking up the parent of a Logger. This
  notably affected the performance of "do-nothing" logging (e.g. when a 
  log message is discarded because it is below a loggers' threshold)
  

# lgr 0.3.2

* Added AppenderSyslog for logging to syslog via 
  [rsyslog](https://github.com/atheriel/rsyslog) (thanks to atheriel)


# lgr 0.3.1

* Added `logger_tree()` which provides an overview of all registered loggers

* Added `print()` and `format()` methods for Appenders

* `AppenderMemory`: added `data` and `dt` active fields (which return the
  log as a data.frame or data.table)

* Removed deprecated functions `FATAL()`, `ERROR()`. Use `lgr$fatal()`, 
  `lgr$error()`, ... instead.

* `AppenderMemory`: `$buffer_dt()` and `$show()` now handle custom fields
  containing atomic vectors correctly


# lgr 0.3.0

* Added support for rotating log files via `AppenderFileRotating`, 
  `AppenderFileRotatingDate` and `AppenderFileRotatingTime`. Requires the
  package [rotor](https://github.com/s-fleck/rotor).
  
* functions like `show_log()`, `show_data()`,... now accept logger names as
  well as Logger or Appender objects as `target`.
  
* `AppenderFile$new()` now creates an empty file, or fails if it can't

* Improved support for RMariaDB and dropped support for RMySQL

* Improved support for RPostgres and dropped support for RPostgreSQL

* added `reset` argument to `get_logger()`. This completely resets the
  configuration of the logger and also replaces special loggers (such as 
  `LoggerGlue`) with vanilla ones.


# lgr 0.2.2

* The root logger can now be configured via `options()` and/or environment 
  variables (see `?lgr`)
  
* `basic_config()` now accepts thresholds ("info", "fatal") as arguments to
  `console` and `memory`. 
  
* The default config of the root logger has changed. It now only has a
  console appender and a default threshold of `"info"`. To get
  back the old behaviour run 
  `basic_config(threshold = "all", console = "info", memory = "all")`.
  
* `$config(NULL)` now resets a Logger to its default/unconfigured state

* `$config()` now accepts YAML as well as JSON files (or YAML/JSON as a 
  character string)
  
* `with_log_level()` and `with_log_value()` now accept logger names as well as 
  Logger objects as the `logger` argument
  
* `get_logger_glue()` now works as intended

* Deprecated `FATAL()`, `ERROR()`. Use `lgr$fatal()`, `lgr$error()`, ... instead.


# lgr 0.2.1

* Emergency fix that ensures test suite cleans up temporary files 

* Removed .rd file for the unexported LoggerRoot class


# lgr 0.2.0

* `get_loggers()` registers new loggers in the lgr::loggers namespace, this 
  is a more global and decoupled approach similar to how python logging handles 
  loggers. 
  
* removed `full_name` active binding for loggers. Loggers now only have 
  qualified names and `name` is now identical to what `full_name` was before.
  For consistency the format method of `ancestry` has also been revised.
  
* Logger inheritance is now derived from the qualified name of a logger. 
  Consequently `lg$parent` is now derived from `lg$name`, `lg$set_parent()` 
  is no longer possible.
  
* If no threshold is set for a new Logger, it now inherits the threshold
  of its parent
  
* Depend on R6 >= 2.4.0 which includes relevant fixes to finalizers. finalize 
  methods are now private.
  
* Logger now have a `config` method that allows configuring Loggers with config
  objects and YAML files (experimental)
  
* added `with_logging()`, the opposite of `without_logging()`. This can be
  handy for automated tests where you might want so switch logging off/on only
  for specific unit tests.


# lgr 0.1.1

* Added `show_data()` and `show_dt()` for quick access to the root loggers
  in memory log as `data.frame` or `data.table` respectively
  
* numerous small fixes

* removed non-breaking-spaces from .RD files. This caused unforeseen problems 
  with the compiling the .pdf manual during the CRAN submission process.
