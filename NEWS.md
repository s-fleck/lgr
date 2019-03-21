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
* If no threshold is set of r a new Logger, it now inherits the threshold
  of its parent
* Depend on R6 >= 2.4.0 which includes relevant fixes to finalizers. finalize 
  methods are now private.
* Logger now have a `config` method that allows configuring Loggers with config
  objecgts and YAML files (experimental)
* added `with_logging()`, the opposite of `without_logging()`. This can be
  handy for automated tests where you might want so switch logging off/on only
  for specific unit tests.


# lgr 0.1.1

* Added `show_data()` and `show_dt()` for quick access to the root loggers
  in memory log as `data.frame` or `data.table` respectively
* numerous small fixes
* removed non-breaking-spaces from .RD files. This caused unforeseen problems 
  with the compiling the .pdf manual during the CRAN submission process.
