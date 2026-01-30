# Log to a date-stamped rotating file

Log to a date-stamped rotating file

Log to a date-stamped rotating file

## Details

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

[`lgr::Filterable`](https://s-fleck.github.io/lgr/reference/Filterable.md)
-\>
[`lgr::Appender`](https://s-fleck.github.io/lgr/reference/Appender.md)
-\>
[`lgr::AppenderFile`](https://s-fleck.github.io/lgr/reference/AppenderFile.md)
-\>
[`lgr::AppenderFileRotating`](https://s-fleck.github.io/lgr/reference/AppenderFileRotating.md)
-\>
[`lgr::AppenderFileRotatingTime`](https://s-fleck.github.io/lgr/reference/AppenderFileRotatingTime.md)
-\> `AppenderFileRotatingDate`

## Methods

### Public methods

- [`AppenderFileRotatingDate$new()`](#method-AppenderFileRotatingDate-new)

- [`AppenderFileRotatingDate$clone()`](#method-AppenderFileRotatingDate-clone)

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
- [`lgr::AppenderFileRotatingTime$format()`](https://s-fleck.github.io/lgr/reference/AppenderFileRotatingTime.html#method-format)
- [`lgr::AppenderFileRotatingTime$rotate()`](https://s-fleck.github.io/lgr/reference/AppenderFileRotatingTime.html#method-rotate)
- [`lgr::AppenderFileRotatingTime$set_age()`](https://s-fleck.github.io/lgr/reference/AppenderFileRotatingTime.html#method-set_age)
- [`lgr::AppenderFileRotatingTime$set_cache_backups()`](https://s-fleck.github.io/lgr/reference/AppenderFileRotatingTime.html#method-set_cache_backups)
- [`lgr::AppenderFileRotatingTime$set_fmt()`](https://s-fleck.github.io/lgr/reference/AppenderFileRotatingTime.html#method-set_fmt)
- [`lgr::AppenderFileRotatingTime$set_overwrite()`](https://s-fleck.github.io/lgr/reference/AppenderFileRotatingTime.html#method-set_overwrite)

------------------------------------------------------------------------

### Method `new()`

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

- `size, age, max_backups, compression, backup_dir, fmt, overwrite, cache_backups`:

  see
  [`rotor::rotate_date()`](https://s-fleck.github.io/rotor/reference/rotate.html)
  for the meaning of these arguments. Note that `fmt` corresponds to
  `format` (because `$format` has a special meaning for R6 classes).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    AppenderFileRotatingDate$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
