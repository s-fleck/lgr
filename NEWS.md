# lgr 0.1.1.9000

* removed `full_name` active binding for loggers. Loggers now only have 
  qualified names and `name` is now identical to what `full_name` was before.
  For consistency the format method of `ancestry` has also been revised.

# lgr 0.1.1

* Added `show_data()` and `show_dt()` for quick access to the root loggers
  in memory log as `data.frame` or `data.table` respectively
* numerous small fixes
* removed non-breaking-spaces from .RD files. This caused unforseen problems 
  with the compiling the .pdf manuall during the CRAN submission process.
