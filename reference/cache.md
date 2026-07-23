# Flush `basemaps` cache

This function flushes the `basemaps` cache and thereby removes all
previously queried and/or composited products from the map directories
(temporary or user-defined using the argument `map_dir`) used during the
current session.

## Usage

``` r
flush_cache()
```

## Value

None.

## Examples

``` r
library(basemaps)
flush_cache()
```
