# Get removed CRAN packages

Get removed CRAN packages

## Usage

``` r
get_removed_cran_packages(
  date_interval = lubridate::interval(lubridate::today() - 7L, lubridate::today()),
  limit = 300L
)
```

## Arguments

- date_interval:

  (Interval)  
  Date Interval
  ([lubridate::interval](https://lubridate.tidyverse.org/reference/interval.html))

- limit:

  (integer)  
  Maximum amount of items to query

## Examples

``` r
if (FALSE) { # \dontrun{
get_removed_cran_packages()
} # }
```
