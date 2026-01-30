# A Fully Featured Logging Framework for R

For details please refer to
[`vignette("lgr", package = "lgr")`](https://s-fleck.github.io/lgr/articles/lgr.md).

## Options

You can also set these options in your `.Rprofile` to make them
permanent. Some options can also be set via environment variables (The
environment variables are only used if the option is not set manually
from R).

- `lgr.colors`:

  a `list` of `functions` used for coloring the log levels in console
  output. Usually these will be functions from the package **crayon**

- `lgr.log_levels`:

  A named `integer` vector of log levels that are known to lgr for
  labeling, setting thresholds, etc... . Instead of modifying this
  option manually use
  [`add_log_levels()`](https://s-fleck.github.io/lgr/reference/get_log_levels.md)
  and
  [`remove_log_levels()`](https://s-fleck.github.io/lgr/reference/get_log_levels.md)

- `lgr.default_threshold`:

  `character` or `integer` scalar. The minimum [log
  level](https://s-fleck.github.io/lgr/reference/get_log_levels.md) that
  should be processed by the root logger. Defaults to `400` (`"info"`),
  or to the value of the environment variable `LGR_DEFAULT_THRESHOLD` if
  it is set. This option overrides the threshold specified in
  `lgr.default_config` if both are set.

- `lgr.default_config`:

  Default configuration for the root logger. Can either be a special
  list object, a path to a YAML file, or a character scalar containing
  YAML code. See
  [logger_config](https://s-fleck.github.io/lgr/reference/logger_config.md)
  for details. Defaults to the value of the environment variable
  `LGR_DEFAULT_CONFIG` if it is set.

- `lgr.suspend_logging`:

  `TRUE` or `FALSE`. Suspend all logging for all loggers. Defaults to
  the `TRUE` if the environment variable `LGR_SUSPEND_LOGGING` is set to
  `"TRUE"`. Instead of modifying this option manually use
  [`suspend_logging()`](https://s-fleck.github.io/lgr/reference/suspend_logging.md)
  and
  [`unsuspend_logging()`](https://s-fleck.github.io/lgr/reference/suspend_logging.md)

- `lgr.user`:

  a `character` scalar. The default username for
  [`lgr::get_user()`](https://s-fleck.github.io/lgr/reference/system_infos.md).

## See also

Useful links:

- <https://s-fleck.github.io/lgr/>

- Report bugs at <https://github.com/s-fleck/lgr/issues/>

## Author

**Maintainer**: Stefan Fleck <stefan.b.fleck@gmail.com>
([ORCID](https://orcid.org/0000-0003-3344-9851))
