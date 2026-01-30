# Log to a rotating file

Log to a rotating file

Log to a rotating file

## Details

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

[`lgr::Filterable`](https://s-fleck.github.io/lgr/reference/Filterable.md)
-\>
[`lgr::Appender`](https://s-fleck.github.io/lgr/reference/Appender.md)
-\>
[`lgr::AppenderFile`](https://s-fleck.github.io/lgr/reference/AppenderFile.md)
-\> `AppenderFileRotating`

## Active bindings

- `backups`:

  A `data.frame` containing information on path, file size, etc... on
  the available backups of `file`.

## Methods

### Public methods

- [`AppenderFileRotating$new()`](#method-AppenderFileRotating-new)

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

- [`AppenderFileRotating$clone()`](#method-AppenderFileRotating-clone)

Inherited methods

- [`lgr::Filterable$add_filter()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-add_filter)
- [`lgr::Filterable$filter()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-filter)
- [`lgr::Filterable$remove_filter()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-remove_filter)
- [`lgr::Filterable$set_filters()`](https://s-fleck.github.io/lgr/reference/Filterable.html#method-set_filters)
- [`lgr::Appender$set_layout()`](https://s-fleck.github.io/lgr/reference/Appender.html#method-set_layout)
- [`lgr::Appender$set_threshold()`](https://s-fleck.github.io/lgr/reference/Appender.html#method-set_threshold)
- [`lgr::AppenderFile$show()`](https://s-fleck.github.io/lgr/reference/AppenderFile.html#method-show)

------------------------------------------------------------------------

### Method `new()`

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

- `size, max_backups, compression, backup_dir, fmt`:

  see
  [`rotor::rotate()`](https://s-fleck.github.io/rotor/reference/rotate.html)
  for the meaning of these arguments. Note that `fmt` corresponds to
  `format` and `backup_dir` to `dir`.

------------------------------------------------------------------------

### Method [`append()`](https://rdrr.io/r/base/append.html)

#### Usage

    AppenderFileRotating$append(event)

------------------------------------------------------------------------

### Method `rotate()`

#### Usage

    AppenderFileRotating$rotate(force = FALSE)

------------------------------------------------------------------------

### Method `prune()`

#### Usage

    AppenderFileRotating$prune(max_backups = self$max_backups)

------------------------------------------------------------------------

### Method `set_file()`

#### Usage

    AppenderFileRotating$set_file(file)

------------------------------------------------------------------------

### Method `set_size()`

#### Usage

    AppenderFileRotating$set_size(x)

------------------------------------------------------------------------

### Method `set_max_backups()`

#### Usage

    AppenderFileRotating$set_max_backups(x)

------------------------------------------------------------------------

### Method `set_compression()`

#### Usage

    AppenderFileRotating$set_compression(x)

------------------------------------------------------------------------

### Method `set_create_file()`

#### Usage

    AppenderFileRotating$set_create_file(x)

------------------------------------------------------------------------

### Method `set_backup_dir()`

#### Usage

    AppenderFileRotating$set_backup_dir(x)

------------------------------------------------------------------------

### Method [`format()`](https://rdrr.io/r/base/format.html)

#### Usage

    AppenderFileRotating$format(color = false, ...)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    AppenderFileRotating$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
