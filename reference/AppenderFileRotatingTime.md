# Log to a time-stamped rotating file

Log to a time-stamped rotating file

Log to a time-stamped rotating file

## Details

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

[`lgr::Filterable`](https://s-fleck.github.io/lgr/reference/Filterable.md)
-\>
[`lgr::Appender`](https://s-fleck.github.io/lgr/reference/Appender.md)
-\>
[`lgr::AppenderFile`](https://s-fleck.github.io/lgr/reference/AppenderFile.md)
-\>
[`lgr::AppenderFileRotating`](https://s-fleck.github.io/lgr/reference/AppenderFileRotating.md)
-\> `AppenderFileRotatingTime`

## Active bindings

- `cache_backups`:

  `TRUE` or `FALSE`. If `TRUE` (the default) the list of backups is
  cached, if `FALSE` it is read from disk every time this appender
  triggers. Caching brings a significant speedup for checking whether to
  rotate or not based on the `age` of the last backup, but is only safe
  if there are no other programs/functions (except this appender)
  interacting with the backups.

## Methods

### Public methods

- [`AppenderFileRotatingTime$new()`](#method-AppenderFileRotatingTime-new)

- [`AppenderFileRotatingTime$rotate()`](#method-AppenderFileRotatingTime-rotate)

- [`AppenderFileRotatingTime$set_age()`](#method-AppenderFileRotatingTime-set_age)

- [`AppenderFileRotatingTime$set_fmt()`](#method-AppenderFileRotatingTime-set_fmt)

- [`AppenderFileRotatingTime$set_overwrite()`](#method-AppenderFileRotatingTime-set_overwrite)

- [`AppenderFileRotatingTime$set_cache_backups()`](#method-AppenderFileRotatingTime-set_cache_backups)

- [`AppenderFileRotatingTime$format()`](#method-AppenderFileRotatingTime-format)

- [`AppenderFileRotatingTime$clone()`](#method-AppenderFileRotatingTime-clone)

Inherited methods

- [`lgr::Filterable$add_filter()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-add_filter)
- [`lgr::Filterable$filter()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-filter)
- [`lgr::Filterable$remove_filter()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-remove_filter)
- [`lgr::Filterable$set_filters()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-set_filters)
- [`lgr::Appender$set_layout()`](https://s-fleck.github.io/lgr/reference/Appender.html#method-set_layout)
- [`lgr::Appender$set_threshold()`](https://s-fleck.github.io/lgr/reference/Appender.html#method-set_threshold)
- [`lgr::AppenderFile$show()`](https://s-fleck.github.io/lgr/reference/AppenderFile.html#method-show)
- [`lgr::AppenderFileRotating$append()`](https://s-fleck.github.io/lgr/reference/AppenderFileRotating.html#method-append)
- [`lgr::AppenderFileRotating$prune()`](https://s-fleck.github.io/lgr/reference/AppenderFileRotating.html#method-prune)
- [`lgr::AppenderFileRotating$set_backup_dir()`](https://s-fleck.github.io/lgr/reference/AppenderFileRotating.html#method-set_backup_dir)
- [`lgr::AppenderFileRotating$set_compression()`](https://s-fleck.github.io/lgr/reference/AppenderFileRotating.html#method-set_compression)
- [`lgr::AppenderFileRotating$set_create_file()`](https://s-fleck.github.io/lgr/reference/AppenderFileRotating.html#method-set_create_file)
- [`lgr::AppenderFileRotating$set_file()`](https://s-fleck.github.io/lgr/reference/AppenderFileRotating.html#method-set_file)
- [`lgr::AppenderFileRotating$set_max_backups()`](https://s-fleck.github.io/lgr/reference/AppenderFileRotating.html#method-set_max_backups)
- [`lgr::AppenderFileRotating$set_size()`](https://s-fleck.github.io/lgr/reference/AppenderFileRotating.html#method-set_size)

------------------------------------------------------------------------

### Method `new()`

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

- `size, age, max_backups, compression, backup_dir, fmt, overwrite, cache_backups`:

  see
  [`rotor::rotate_time()`](https://s-fleck.github.io/rotor/reference/rotate.html)
  for the meaning of these arguments. Note that `fmt` corresponds to
  `format` and `backup_dir` to `dir`.

------------------------------------------------------------------------

### Method `rotate()`

#### Usage

    AppenderFileRotatingTime$rotate(force = FALSE, now = Sys.time())

------------------------------------------------------------------------

### Method `set_age()`

#### Usage

    AppenderFileRotatingTime$set_age(x)

------------------------------------------------------------------------

### Method `set_fmt()`

#### Usage

    AppenderFileRotatingTime$set_fmt(x)

------------------------------------------------------------------------

### Method `set_overwrite()`

#### Usage

    AppenderFileRotatingTime$set_overwrite(x)

------------------------------------------------------------------------

### Method `set_cache_backups()`

set the `cache_backups` flag.

#### Usage

    AppenderFileRotatingTime$set_cache_backups(x)

#### Arguments

- `x`:

  a `logical` scalar

------------------------------------------------------------------------

### Method [`format()`](https://rdrr.io/r/base/format.html)

#### Usage

    AppenderFileRotatingTime$format(color = FALSE, ...)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    AppenderFileRotatingTime$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
