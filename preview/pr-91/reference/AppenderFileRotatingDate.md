# Log to a date-stamped rotating file

This is a simpler version of AppenderFileRotatingTime when the
timestamps do not need to include sub-day accuracy.

## See also

[AppenderFileRotatingTime](https://s-fleck.github.io/lgr/reference/AppenderFileRotatingTime.md),
[AppenderFileRotating](https://s-fleck.github.io/lgr/reference/AppenderFileRotating.md),
[`rotor::rotate()`](https://s-fleck.github.io/rotor/reference/rotate.html)

Other Appenders:
[`Appender`](https://s-fleck.github.io/lgr/reference/Appender.md),
[`AppenderBuffer`](https://s-fleck.github.io/lgr/reference/AppenderBuffer.md),
[`AppenderConsole`](https://s-fleck.github.io/lgr/reference/AppenderConsole.md),
[`AppenderFile`](https://s-fleck.github.io/lgr/reference/AppenderFile.md),
[`AppenderFileRotating`](https://s-fleck.github.io/lgr/reference/AppenderFileRotating.md),
[`AppenderFileRotatingTime`](https://s-fleck.github.io/lgr/reference/AppenderFileRotatingTime.md),
[`AppenderTable`](https://s-fleck.github.io/lgr/reference/AppenderTable.md)

## Super classes

[`Filterable`](https://s-fleck.github.io/lgr/reference/Filterable.md)
-\> [`Appender`](https://s-fleck.github.io/lgr/reference/Appender.md)
-\>
[`AppenderFile`](https://s-fleck.github.io/lgr/reference/AppenderFile.md)
-\>
[`AppenderFileRotating`](https://s-fleck.github.io/lgr/reference/AppenderFileRotating.md)
-\>
[`AppenderFileRotatingTime`](https://s-fleck.github.io/lgr/reference/AppenderFileRotatingTime.md)
-\> `AppenderFileRotatingDate`

## Methods

### Public methods

- [`AppenderFileRotatingDate$new()`](#method-AppenderFileRotatingDate-initialize)

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
- [`AppenderFileRotatingTime$format()`](https://s-fleck.github.io/lgr/reference/AppenderFileRotatingTime.html#method-format)
- [`AppenderFileRotatingTime$rotate()`](https://s-fleck.github.io/lgr/reference/AppenderFileRotatingTime.html#method-rotate)
- [`AppenderFileRotatingTime$set_age()`](https://s-fleck.github.io/lgr/reference/AppenderFileRotatingTime.html#method-set_age)
- [`AppenderFileRotatingTime$set_cache_backups()`](https://s-fleck.github.io/lgr/reference/AppenderFileRotatingTime.html#method-set_cache_backups)
- [`AppenderFileRotatingTime$set_fmt()`](https://s-fleck.github.io/lgr/reference/AppenderFileRotatingTime.html#method-set_fmt)
- [`AppenderFileRotatingTime$set_overwrite()`](https://s-fleck.github.io/lgr/reference/AppenderFileRotatingTime.html#method-set_overwrite)

------------------------------------------------------------------------

### `AppenderFileRotatingDate$new()`

Create a new AppenderFileRotatingDate.

#### Usage

    AppenderFileRotatingDate$new(
      file,
      threshold = NA_integer_,
      layout = LayoutFormat$new(),
      filters = NULL,
      age = Inf,
      size = -1,
      max_backups = Inf,
      compression = FALSE,
      backup_dir = dirname(file),
      fmt = "%Y-%m-%d",
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
  [`rotor::rotate_date()`](https://s-fleck.github.io/rotor/reference/rotate.html)
  for the meaning of these arguments. Note that `backup_dir` corresponds
  to `dir`.

- `create_file`:

  Defunct, do not use.
