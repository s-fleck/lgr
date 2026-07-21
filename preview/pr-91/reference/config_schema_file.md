# Location of the shipped lgr configuration JSON Schema

lgr ships a [JSON Schema](https://json-schema.org/) describing its
declarative configuration format (see
[config](https://s-fleck.github.io/lgr/reference/config.md)) for use by
editors (e.g. the VS Code YAML/JSON Schema extensions) to provide
autocompletion and inline validation.

## Usage

``` r
config_schema_file()
```

## Value

a `character` scalar: the path to the installed JSON Schema file.

## Examples

``` r
config_schema_file()
#> [1] "/home/runner/work/_temp/Library/lgr/lgr-config-schema.json"
```
