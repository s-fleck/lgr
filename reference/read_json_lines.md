# Read a JSON logfile

Read a JSON logfile

## Usage

``` r
read_json_lines(file, ...)
```

## Arguments

- file:

  `character` scalar. path to a JSON logfile (one JSON object per line)

- ...:

  passed on to
  [`jsonlite::stream_in()`](https://jeroen.r-universe.dev/jsonlite/reference/stream_in.html)

## Value

a `data.frame`

## See also

[LayoutJson](https://s-fleck.github.io/lgr/reference/LayoutJson.md)
