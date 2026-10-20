# Inject standard runtime metadata into all events processed by a Logger/Appender

Like
[FilterInject](https://s-fleck.github.io/lgr/reference/FilterInject.md),
but automatically computes and injects a bundle of standard runtime
metadata once, at construction time: `hostName` and `rVersion` are
always included; `containerId` is included only when running inside a
Docker container; `scriptFile` is included only when the currently
running script's path can be determined (via RStudio,
`Rscript`/`R --file`, or
[`source()`](https://rdrr.io/r/base/source.html)). Any field that cannot
be determined is omitted entirely, never set to `NA` or an empty string.

Pass additional named values via `...`/`.list` exactly as with
[FilterInject](https://s-fleck.github.io/lgr/reference/FilterInject.md)
– they are injected alongside the computed bundle, and a name that
matches one of the computed fields (`hostName`, `rVersion`,
`containerId`, `scriptFile`) overrides the computed value rather than
causing a duplicate-field error.

Vendor/deployment-specific values (e.g. a release id from a CI/CD tool)
are intentionally *not* part of the computed bundle – inject them the
same way as any other custom value, typically via environment variable
interpolation in a declarative config (e.g.
`releaseId: "${RELEASE_ID:-}"`).

## Super classes

[`EventFilter`](https://s-fleck.github.io/lgr/reference/EventFilter.md)
-\>
[`FilterInject`](https://s-fleck.github.io/lgr/reference/FilterInject.md)
-\> `FilterInjectSystemInfo`

## Methods

### Public methods

- [`FilterInjectSystemInfo$new()`](#method-FilterInjectSystemInfo-initialize)

- [`FilterInjectSystemInfo$clone()`](#method-FilterInjectSystemInfo-clone)

------------------------------------------------------------------------

### `FilterInjectSystemInfo$new()`

Initialize a new FilterInjectSystemInfo

#### Usage

    FilterInjectSystemInfo$new(..., .list = list())

#### Arguments

- `..., .list`:

  any number of named R objects to inject in addition to (or, if
  same-named, instead of) the computed metadata bundle. See
  [FilterInject](https://s-fleck.github.io/lgr/reference/FilterInject.md).

------------------------------------------------------------------------

### `FilterInjectSystemInfo$clone()`

The objects of this class are cloneable with this method.

#### Usage

    FilterInjectSystemInfo$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
lg <- get_logger("test")
lg$add_filter(FilterInjectSystemInfo$new(), "systemInfo")
lg$info("an event with hostName/rVersion/... attached")
#> INFO  [15:02:38.118] an event with hostName/rVersion/... attached {scriptFile: `/home/runner/work/_temp/a10e8428-a821-47fb-9709-59d765df57f3`, rVersion: `R version 4.6.1 (2026-06-24)`, hostName: `runnervm8k3b6`}
lg$config(NULL)  # reset config
```
