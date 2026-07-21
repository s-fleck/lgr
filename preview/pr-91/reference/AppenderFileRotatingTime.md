# Log to a time-stamped rotating file

An extension of
[AppenderFile](https://s-fleck.github.io/lgr/reference/AppenderFile.md)
that rotates logfiles based on certain conditions. Please refer to the
documentation of
[`rotor::rotate_time()`](https://s-fleck.github.io/rotor/reference/rotate.html)
for the meanings of the extra arguments

## See also

[AppenderFileRotatingDate](https://s-fleck.github.io/lgr/reference/AppenderFileRotatingDate.md),
[AppenderFileRotating](https://s-fleck.github.io/lgr/reference/AppenderFileRotating.md),
[`rotor::rotate()`](https://s-fleck.github.io/rotor/reference/rotate.html)

Other Appenders:
[`Appender`](https://s-fleck.github.io/lgr/reference/Appender.md),
[`AppenderBuffer`](https://s-fleck.github.io/lgr/reference/AppenderBuffer.md),
[`AppenderConsole`](https://s-fleck.github.io/lgr/reference/AppenderConsole.md),
[`AppenderFile`](https://s-fleck.github.io/lgr/reference/AppenderFile.md),
[`AppenderFileRotating`](https://s-fleck.github.io/lgr/reference/AppenderFileRotating.md),
[`AppenderFileRotatingDate`](https://s-fleck.github.io/lgr/reference/AppenderFileRotatingDate.md),
[`AppenderTable`](https://s-fleck.github.io/lgr/reference/AppenderTable.md)

## Super classes

[`Filterable`](https://s-fleck.github.io/lgr/reference/Filterable.md)
-\> [`Appender`](https://s-fleck.github.io/lgr/reference/Appender.md)
-\>
[`AppenderFile`](https://s-fleck.github.io/lgr/reference/AppenderFile.md)
-\>
[`AppenderFileRotating`](https://s-fleck.github.io/lgr/reference/AppenderFileRotating.md)
-\> `AppenderFileRotatingTime`

## Active bindings

- `age`:

  The maximum age of the log file that triggers rotation. See
  `$set_age()`.

- `overwrite`:

  Whether an existing backup for the same timestamp may be overwritten.
  See `$set_overwrite()`.

- `fmt`:

  The [`strftime()`](https://rdrr.io/r/base/strptime.html)-style
  timestamp format used for naming backups. See `$set_fmt()`.

- `cache_backups`:

  `TRUE` or `FALSE`. If `TRUE` (the default) the list of backups is
  cached, if `FALSE` it is read from disk every time this appender
  triggers. Caching brings a significant speedup for checking whether to
  rotate or not based on the `age` of the last backup, but is only safe
  if there are no other programs/functions (except this appender)
  interacting with the backups.

## Methods

### Public methods

- [`AppenderFileRotatingTime$new()`](#method-AppenderFileRotatingTime-initialize)

- [`AppenderFileRotatingTime$rotate()`](#method-AppenderFileRotatingTime-rotate)

- [`AppenderFileRotatingTime$set_age()`](#method-AppenderFileRotatingTime-set_age)

- [`AppenderFileRotatingTime$set_fmt()`](#method-AppenderFileRotatingTime-set_fmt)

- [`AppenderFileRotatingTime$set_overwrite()`](#method-AppenderFileRotatingTime-set_overwrite)

- [`AppenderFileRotatingTime$set_cache_backups()`](#method-AppenderFileRotatingTime-set_cache_backups)

- [`AppenderFileRotatingTime$format()`](#method-AppenderFileRotatingTime-format)

Inherited methods

- [`Filterable$add_filter()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-add_filter)
- [`Filterable$filter()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-filter)
- [`Filterable$remove_filter()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-remove_filter)
- [`Filterable$set_filters()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-set_filters)
- [`Appender$set_layout()`](https://s-fleck.github.io/lgr/reference/Appender.html#method-set_layout)
- [`Appender$set_threshold()`](https://s-fleck.github.io/lgr/reference/Appender.html#method-set_threshold)
- [`AppenderFile$show()`](https://s-fleck.github.io/lgr/reference/AppenderFile.html#method-show)
- [`AppenderFileRotating$append()`](https://s-fleck.github.io/lgr/reference/AppenderFileRotating.html#method-append)
- [`AppenderFileRotating$prune()`](https://s-fleck.github.io/lgr/reference/AppenderFileRotating.html#method-prune)
- [`AppenderFileRotating$set_backup_dir()`](https://s-fleck.github.io/lgr/reference/AppenderFileRotating.html#method-set_backup_dir)
- [`AppenderFileRotating$set_compression()`](https://s-fleck.github.io/lgr/reference/AppenderFileRotating.html#method-set_compression)
- [`AppenderFileRotating$set_create_file()`](https://s-fleck.github.io/lgr/reference/AppenderFileRotating.html#method-set_create_file)
- [`AppenderFileRotating$set_file()`](https://s-fleck.github.io/lgr/reference/AppenderFileRotating.html#method-set_file)
- [`AppenderFileRotating$set_max_backups()`](https://s-fleck.github.io/lgr/reference/AppenderFileRotating.html#method-set_max_backups)
- [`AppenderFileRotating$set_size()`](https://s-fleck.github.io/lgr/reference/AppenderFileRotating.html#method-set_size)

------------------------------------------------------------------------

### `AppenderFileRotatingTime$new()`

Create a new AppenderFileRotatingTime.

#### Usage

    AppenderFileRotatingTime$new(
      file,
      threshold = NA_integer_,
      layout = LayoutFormat$new(),
      filters = NULL,
      age = Inf,
      size = -1,
      max_backups = Inf,
      compression = FALSE,
      backup_dir = dirname(file),
      fmt = "%Y-%m-%d--%H-%M-%S",
      overwrite = FALSE,
      cache_backups = TRUE,
      create_file = NULL
    )

#### Arguments

- `file`:

  `character` scalar. Path to the log file. If `file` does not exist it
  will be created.

- `threshold`:

  `character` or `integer` scalar. The minimum log level that triggers
  this Appender. See
  [log_levels](https://s-fleck.github.io/lgr/reference/get_log_levels.md).

- `layout`:

  A [Layout](https://s-fleck.github.io/lgr/reference/Layout.md) that
  determines how
  [LogEvents](https://s-fleck.github.io/lgr/reference/LogEvent.md) are
  formatted before being output by this Appender.

- `filters`:

  A `list` of
  [EventFilters](https://s-fleck.github.io/lgr/reference/EventFilter.md)
  or predicate functions used to filter events for this Appender. See
  [Filterable](https://s-fleck.github.io/lgr/reference/Filterable.md).

- `age, size, max_backups, compression, backup_dir, fmt, overwrite, cache_backups`:

  see
  [`rotor::rotate_time()`](https://s-fleck.github.io/rotor/reference/rotate.html)
  for the meaning of these arguments. Note that `backup_dir` corresponds
  to `dir`.

- `create_file`:

  Defunct, do not use.

------------------------------------------------------------------------

### `AppenderFileRotatingTime$rotate()`

Rotate the log file if it meets the rotation conditions (see `$size`,
`$age`), or unconditionally if `force = TRUE`. See
[`rotor::rotate_time()`](https://s-fleck.github.io/rotor/reference/rotate.html)
for details.

#### Usage

    AppenderFileRotatingTime$rotate(force = FALSE, now = Sys.time())

#### Arguments

- `force`:

  `TRUE` or `FALSE`. Force rotation regardless of whether the rotation
  conditions are met.

- `now`:

  A `POSIXct` scalar. The timestamp to use for naming the new backup and
  for evaluating `$age`.

------------------------------------------------------------------------

### `AppenderFileRotatingTime$set_age()`

Set the maximum age of the log file that triggers rotation. See
[`rotor::rotate_time()`](https://s-fleck.github.io/rotor/reference/rotate.html).

#### Usage

    AppenderFileRotatingTime$set_age(x)

#### Arguments

- `x`:

  A `difftime` or `numeric` scalar (interpreted as days), or `Inf` to
  disable age-based rotation.

------------------------------------------------------------------------

### `AppenderFileRotatingTime$set_fmt()`

Set the [`strftime()`](https://rdrr.io/r/base/strptime.html)-style
timestamp format used for naming backups. See
[`rotor::rotate_time()`](https://s-fleck.github.io/rotor/reference/rotate.html).

#### Usage

    AppenderFileRotatingTime$set_fmt(x)

#### Arguments

- `x`:

  `character` scalar.

------------------------------------------------------------------------

### `AppenderFileRotatingTime$set_overwrite()`

Set whether an existing backup for the same timestamp may be
overwritten. See
[`rotor::rotate_time()`](https://s-fleck.github.io/rotor/reference/rotate.html).

#### Usage

    AppenderFileRotatingTime$set_overwrite(x)

#### Arguments

- `x`:

  `TRUE` or `FALSE`.

------------------------------------------------------------------------

### `AppenderFileRotatingTime$set_cache_backups()`

set the `cache_backups` flag.

#### Usage

    AppenderFileRotatingTime$set_cache_backups(x)

#### Arguments

- `x`:

  a `logical` scalar

------------------------------------------------------------------------

### `AppenderFileRotatingTime$format()`

Format the Appender for printing (see also
[`print.Appender()`](https://s-fleck.github.io/lgr/reference/print.Appender.md)).

#### Usage

    AppenderFileRotatingTime$format(color = FALSE, ...)

#### Arguments

- `color`:

  `TRUE` or `FALSE`. Output with color? Requires the package **crayon**.

- `...`:

  ignored
