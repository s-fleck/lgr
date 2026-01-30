# Abstract Class for Filterables

Superclass for classes that have a `$filter()` method such as
[Appenders](https://s-fleck.github.io/lgr/reference/Appender.md) and
[Loggers](https://s-fleck.github.io/lgr/reference/Logger.md). See
[EventFilter](https://s-fleck.github.io/lgr/reference/EventFilter.md)
for details.

**NOTE**: This is an *abstract class*. Abstract classes cannot be
instantiated directly, but are exported for package developers that want
to extend lgr - for example by creating their own
[Appenders](https://s-fleck.github.io/lgr/reference/Appender.md) or
[Layouts](https://s-fleck.github.io/lgr/reference/Layout.md). Please
refer to the *see also* section for actual implementations of this
class.

## See also

Other abstract classes:
[`Appender`](https://s-fleck.github.io/lgr/reference/Appender.md),
[`AppenderMemory`](https://s-fleck.github.io/lgr/reference/AppenderMemory.md),
[`AppenderTable`](https://s-fleck.github.io/lgr/reference/AppenderTable.md)

## Active bindings

- `filters`:

  a `list` of all attached Filters.

## Methods

### Public methods

- [`Filterable$filter()`](#method-Filterable-filter)

- [`Filterable$add_filter()`](#method-Filterable-add_filter)

- [`Filterable$remove_filter()`](#method-Filterable-remove_filter)

- [`Filterable$set_filters()`](#method-Filterable-set_filters)

------------------------------------------------------------------------

### Method [`filter()`](https://rdrr.io/r/stats/filter.html)

Determine whether the LogEvent `x` should be passed on to Appenders
(`TRUE`) or not (`FALSE`). See also the active binding `filters`.

#### Usage

    Filterable$filter(event)

#### Arguments

- `event`:

  a [LogEvent](https://s-fleck.github.io/lgr/reference/LogEvent.md)

------------------------------------------------------------------------

### Method `add_filter()`

Attach a filter

#### Usage

    Filterable$add_filter(filter, name = NULL)

#### Arguments

- `filter`:

  - a function with the single argument `event` that returns `TRUE` or
    `FALSE`;

  - an
    [EventFilter](https://s-fleck.github.io/lgr/reference/EventFilter.md)
    [R6::R6](https://r6.r-lib.org/reference/R6Class.html) object; or

  - any R object with a `$filter()` method.

  If a Filter returns a non-`FALSE` value, will be interpreted as `TRUE`
  (= no filtering takes place) and a warning will be thrown.

- `name`:

  `character` scalar or `NULL`. An optional name which makes it easier
  to access (or remove) the filter

------------------------------------------------------------------------

### Method `remove_filter()`

Remove a filter

#### Usage

    Filterable$remove_filter(pos)

#### Arguments

- `pos`:

  `character` or `integer` scalar. The name or index of the Filter to be
  removed.

------------------------------------------------------------------------

### Method `set_filters()`

Set or replace (all) Filters of parent object. See
[EventFilter](https://s-fleck.github.io/lgr/reference/EventFilter.md)
for how Filters work.

#### Usage

    Filterable$set_filters(filters)

#### Arguments

- `filters`:

  a `list` (named or unnamed) of
  [EventFilters](https://s-fleck.github.io/lgr/reference/EventFilter.md)
  or predicate functions. See
  [`is_filter()`](https://s-fleck.github.io/lgr/reference/is_filter.md).
