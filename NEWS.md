# lgr 0.2.0.9000

* to be released as 1.0.0

* `get_loggers()` registeres new loggers in the lgr::loggers namespace, this 
  is a more global and decoupled approach to loggers. 
* `lg$parent` is now derived from `lg$name`, `lg$set_parent()` is no longer
  possible.
* removed `full_name` active binding for loggers. Loggers now only have 
  qualified names and `name` is now identical to what `full_name` was before.
  For consistency the format method of `ancestry` has also been revised.
* New Loggers now inherit the log level of their parent unless one is set
* added a `config` method for Loggers.
* Depend on R6 >= 2.4.0 (which includes relevant fixes to finalizers)
* finalize methods are now private
* support for new ways to configure loggers, including YAML config files 
  (experimental)
* added `with_logging()`, the opposite of `without_logging()`. This can be
  handy for automated tests


# lgr 0.1.1

* Added `show_data()` and `show_dt()` for quick access to the root loggers
  in memory log as `data.frame` or `data.table` respectively
* numerous small fixes
* removed non-breaking-spaces from .RD files. This caused unforseen problems 
  with the compiling the .pdf manuall during the CRAN submission process.
