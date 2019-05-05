# lgr 0.2.1.9000

* The root logger can now be configured via `options()` and/or environment 
  variables (see `?lgr`)
* The default config of the root logger has changed. It now only has a
  console appender and a default threshold of `"info"`. To get
  back the old behaviour (`"info"` console appender, `"all"` in-memory appender) 
  run `basic_config()` with default arguments.
* `basic_config()` now accepts thresholds ("info", "fatal") as arguments to
  `console` and `memory`. 
* `$config(NULL)` now resets a Logger to its default/unconfigured state
* `$config()` now accepts YAML as well as JSON files or code
* `with_log_level()` and `with_log_value()` now accept logger names as well as 
  Logger objects as the `logger` argument
* `get_logger_glue()` now works as intended
  


# lgr 0.2.1 (2019-03-25)

* Emergency fix that ensures test suite cleans up temporary files 
* Removed .rd file for the unexported LoggerRoot class


# lgr 0.2.0 (2019-03-22)

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


# lgr 0.1.1 (2019-01-30)

* Added `show_data()` and `show_dt()` for quick access to the root loggers
  in memory log as `data.frame` or `data.table` respectively
* numerous small fixes
* removed non-breaking-spaces from .RD files. This caused unforeseen problems 
  with the compiling the .pdf manual during the CRAN submission process.
