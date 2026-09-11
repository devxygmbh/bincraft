# Get new CRAN packages

Get new CRAN packages

## Usage

``` r
get_new_cran_packages(
  date_interval = lubridate::interval(lubridate::today() - 7L, lubridate::today())
)
```

## Arguments

- date_interval:

  (Interval)  
  Date Interval
  ([lubridate::interval](https://lubridate.tidyverse.org/reference/interval.html))

## Examples

``` r
if (FALSE) { # \dontrun{
# last week
get_new_cran_packages()
} # }
```
