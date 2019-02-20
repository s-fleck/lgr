# lgr 0.1.1.9000

* `get_loggers()` registeres new loggers in the lgr::loggers namespace, this 
  is a more global and decoupled approach to loggers. 
* `lg$parent` is now derived from `lg$name`, `lg$set_parent()` is no longer
  possible.
* removed `full_name` active binding for loggers. Loggers now only have 
  qualified names and `name` is now identical to what `full_name` was before.
  For consistency the format method of `ancestry` has also been revised.
* New Loggers now inherit the log level of their parent unless one is set


# lgr 0.1.1

* Added `show_data()` and `show_dt()` for quick access to the root loggers
  in memory log as `data.frame` or `data.table` respectively
* numerous small fixes
* removed non-breaking-spaces from .RD files. This caused unforseen problems 
  with the compiling the .pdf manuall during the CRAN submission process.
