# Log to a rotating file

An extension of
[AppenderFile](https://s-fleck.github.io/lgr/reference/AppenderFile.md)
that rotates logfiles based on certain conditions. Please refer to the
documentation of
[`rotor::rotate()`](https://s-fleck.github.io/rotor/reference/rotate.html)
for the meanings of the extra arguments

## See also

[AppenderFileRotatingDate](https://s-fleck.github.io/lgr/reference/AppenderFileRotatingDate.md),
[AppenderFileRotatingTime](https://s-fleck.github.io/lgr/reference/AppenderFileRotatingTime.md),
[`rotor::rotate()`](https://s-fleck.github.io/rotor/reference/rotate.html)

Other Appenders:
[`Appender`](https://s-fleck.github.io/lgr/reference/Appender.md),
[`AppenderBuffer`](https://s-fleck.github.io/lgr/reference/AppenderBuffer.md),
[`AppenderConsole`](https://s-fleck.github.io/lgr/reference/AppenderConsole.md),
[`AppenderFile`](https://s-fleck.github.io/lgr/reference/AppenderFile.md),
[`AppenderFileRotatingDate`](https://s-fleck.github.io/lgr/reference/AppenderFileRotatingDate.md),
[`AppenderFileRotatingTime`](https://s-fleck.github.io/lgr/reference/AppenderFileRotatingTime.md),
[`AppenderTable`](https://s-fleck.github.io/lgr/reference/AppenderTable.md)

## Super classes

[`Filterable`](https://s-fleck.github.io/lgr/reference/Filterable.md)
-\> [`Appender`](https://s-fleck.github.io/lgr/reference/Appender.md)
-\>
[`AppenderFile`](https://s-fleck.github.io/lgr/reference/AppenderFile.md)
-\> `AppenderFileRotating`

## Active bindings

- `size`:

  The maximum size of the log file that triggers rotation. See
  `$set_size()`.

- `create_file`:

  Defunct, do not use.

- `compression`:

  Whether backups are compressed. See `$set_compression()`.

- `max_backups`:

  The maximum number of backups to keep. See `$set_max_backups()`.

- `backups`:

  A `data.frame` containing information on path, file size, etc... on
  the available backups of `file`.

- `backup_dir`:

  The directory that backups are stored in. See `$set_backup_dir()`.

## Methods

### Public methods

- [`AppenderFileRotating$new()`](#method-AppenderFileRotating-initialize)

- [`AppenderFileRotating$append()`](#method-AppenderFileRotating-append)

- [`AppenderFileRotating$rotate()`](#method-AppenderFileRotating-rotate)

- [`AppenderFileRotating$prune()`](#method-AppenderFileRotating-prune)

- [`AppenderFileRotating$set_file()`](#method-AppenderFileRotating-set_file)

- [`AppenderFileRotating$set_size()`](#method-AppenderFileRotating-set_size)

- [`AppenderFileRotating$set_max_backups()`](#method-AppenderFileRotating-set_max_backups)

- [`AppenderFileRotating$set_compression()`](#method-AppenderFileRotating-set_compression)

- [`AppenderFileRotating$set_create_file()`](#method-AppenderFileRotating-set_create_file)

- [`AppenderFileRotating$set_backup_dir()`](#method-AppenderFileRotating-set_backup_dir)

- [`AppenderFileRotating$format()`](#method-AppenderFileRotating-format)

Inherited methods

- [`Filterable$add_filter()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-add_filter)
- [`Filterable$filter()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-filter)
- [`Filterable$remove_filter()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-remove_filter)
- [`Filterable$set_filters()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-set_filters)
- [`Appender$set_layout()`](https://s-fleck.github.io/lgr/reference/Appender.html#method-set_layout)
- [`Appender$set_threshold()`](https://s-fleck.github.io/lgr/reference/Appender.html#method-set_threshold)
- [`AppenderFile$show()`](https://s-fleck.github.io/lgr/reference/AppenderFile.html#method-show)

------------------------------------------------------------------------

### `AppenderFileRotating$new()`

Create a new AppenderFileRotating.

#### Usage

    AppenderFileRotating$new(
      file,
      threshold = NA_integer_,
      layout = LayoutFormat$new(),
      filters = NULL,
      size = Inf,
      max_backups = Inf,
      compression = FALSE,
      backup_dir = dirname(file),
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

- `size, max_backups, compression, backup_dir`:

  see
  [`rotor::rotate()`](https://s-fleck.github.io/rotor/reference/rotate.html)
  for the meaning of these arguments. Note that `backup_dir` corresponds
  to `dir`.

- `create_file`:

  Defunct, do not use.

------------------------------------------------------------------------

### `AppenderFileRotating$append()`

Process a
[LogEvent](https://s-fleck.github.io/lgr/reference/LogEvent.md) `event`
by appending it to `$file`, then rotate the file if necessary (see
`$rotate()`). This method is usually not called by the user, but invoked
by a [Logger](https://s-fleck.github.io/lgr/reference/Logger.md).

#### Usage

    AppenderFileRotating$append(event)

#### Arguments

- `event`:

  a [LogEvent](https://s-fleck.github.io/lgr/reference/LogEvent.md)

------------------------------------------------------------------------

### `AppenderFileRotating$rotate()`

Rotate the log file if it meets the rotation conditions (see `$size`),
or unconditionally if `force = TRUE`. See
[`rotor::rotate()`](https://s-fleck.github.io/rotor/reference/rotate.html)
for details.

#### Usage

    AppenderFileRotating$rotate(force = FALSE)

#### Arguments

- `force`:

  `TRUE` or `FALSE`. Force rotation regardless of whether the rotation
  conditions are met.

------------------------------------------------------------------------

### `AppenderFileRotating$prune()`

Remove old backups so that at most `max_backups` remain. See
[`rotor::rotate()`](https://s-fleck.github.io/rotor/reference/rotate.html).

#### Usage

    AppenderFileRotating$prune(max_backups = self$max_backups)

#### Arguments

- `max_backups`:

  `integer` scalar. Defaults to `$max_backups`.

------------------------------------------------------------------------

### `AppenderFileRotating$set_file()`

Set a log file

#### Usage

    AppenderFileRotating$set_file(file)

#### Arguments

- `file`:

  `character` scalar. Path to the log file. If `file` does not exist it
  will be created.

------------------------------------------------------------------------

### `AppenderFileRotating$set_size()`

Set the maximum size of the log file that triggers rotation. See
[`rotor::rotate()`](https://s-fleck.github.io/rotor/reference/rotate.html).

#### Usage

    AppenderFileRotating$set_size(x)

#### Arguments

- `x`:

  A `numeric` file size in bytes, a `character` scalar such as `"5mb"`
  (see
  [`rotor::rotate()`](https://s-fleck.github.io/rotor/reference/rotate.html)),
  or `Inf` to disable size-based rotation.

------------------------------------------------------------------------

### `AppenderFileRotating$set_max_backups()`

Set the maximum number of backups to keep. See
[`rotor::rotate()`](https://s-fleck.github.io/rotor/reference/rotate.html).

#### Usage

    AppenderFileRotating$set_max_backups(x)

#### Arguments

- `x`:

  `integer` scalar `>= 0`, or `Inf` to keep all backups.

------------------------------------------------------------------------

### `AppenderFileRotating$set_compression()`

Set whether backups should be compressed. See
[`rotor::rotate()`](https://s-fleck.github.io/rotor/reference/rotate.html).

#### Usage

    AppenderFileRotating$set_compression(x)

#### Arguments

- `x`:

  `TRUE` or `FALSE`.

------------------------------------------------------------------------

### `AppenderFileRotating$set_create_file()`

Defunct, do not use.

#### Usage

    AppenderFileRotating$set_create_file(x)

#### Arguments

- `x`:

  ignored

------------------------------------------------------------------------

### `AppenderFileRotating$set_backup_dir()`

Set the directory that backups are stored in. See
[`rotor::rotate()`](https://s-fleck.github.io/rotor/reference/rotate.html).

#### Usage

    AppenderFileRotating$set_backup_dir(x)

#### Arguments

- `x`:

  `character` scalar. Path to the backup directory.

------------------------------------------------------------------------

### `AppenderFileRotating$format()`

Format the Appender for printing (see also
[`print.Appender()`](https://s-fleck.github.io/lgr/reference/print.Appender.md)).

#### Usage

    AppenderFileRotating$format(color = FALSE, ...)

#### Arguments

- `color`:

  `TRUE` or `FALSE`. Output with color? Requires the package **crayon**.

- `...`:

  ignored
