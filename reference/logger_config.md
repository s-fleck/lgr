# Logger Configuration Objects

`logger_config()` is an S3 constructor for `logger_config` objects that
can be passed to the `$config` method of a
[Logger](https://s-fleck.github.io/lgr/reference/Logger.md). You can
just pass a normal `list` instead, but using this constructor is a more
formal way that includes additional argument checking.

## Usage

``` r
logger_config(
  appenders = NULL,
  threshold = NULL,
  filters = NULL,
  exception_handler = NULL,
  propagate = TRUE
)

as_logger_config(x)

# S3 method for class 'list'
as_logger_config(x)

# S3 method for class 'character'
as_logger_config(x)
```

## Arguments

- appenders:

  see [Logger](https://s-fleck.github.io/lgr/reference/Logger.md)

- threshold:

  see [Logger](https://s-fleck.github.io/lgr/reference/Logger.md)

- filters:

  see [Logger](https://s-fleck.github.io/lgr/reference/Logger.md)

- exception_handler:

  see [Logger](https://s-fleck.github.io/lgr/reference/Logger.md)

- propagate:

  see [Logger](https://s-fleck.github.io/lgr/reference/Logger.md)

- x:

  any R object. Especially:

  - A `character` scalar. This can either be the path to a YAML file or
    a character scalar containing valid YAML

  - a list containing the elements `appenders`, `threshold`,
    `exception_handler`, `propagate` and `filters`. See the section
    *Fields* in
    [Logger](https://s-fleck.github.io/lgr/reference/Logger.md) for
    details.

  - a Logger object, to clone its configuration.

## Value

a `list` with the subclass `"logger_config"`

a logger_config object

## See also

<https://yaml.org/>
