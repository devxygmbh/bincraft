# Get updated CRAN packages

Get updated CRAN packages

## Usage

``` r
get_updated_cran_packages(
  date_interval = lubridate::interval(lubridate::today() - 7L, lubridate::today()),
  limit = 2000L
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
get_updated_cran_packages()
#>               name  version       date
#> 1     huggingfaceR    2.2.0 2026-08-29
#> 2         tidydann    1.0.2 2026-08-29
#> 3    fitdistrBayes    0.2.2 2026-08-29
#> 4         cox.rvph    0.2.0 2026-08-29
#> 5  autoslider.core    0.3.3 2026-08-29
#> 6             sstn    1.0.2 2026-08-29
#> 7           ipeval    0.1.1 2026-08-29
#> 8        GetTDData    1.7.0 2026-08-29
#> 9         gsDesign   3.11.0 2026-08-29
#> 10        tabxplor    2.0.0 2026-08-29
#> 11     yulab.utils    0.2.5 2026-08-29
#> 12      rstantools    2.7.1 2026-08-29
#> 13           safer    0.2.3 2026-08-29
#> 14       autograph    1.2.2 2026-08-29
#> 15       AzureAuth    1.3.5 2026-08-29
#> 16        rasterDT    0.3.3 2026-08-29
#> 17            uwot    0.2.5 2026-08-29
#> 18       tectonicr    0.4.9 2026-08-29
#> 19    bibliometrix    5.5.0 2026-08-29
#> 20         clinfun    1.1.6 2026-08-29
#> 21        spsurvey    5.7.0 2026-08-29
#> 22           kDGLM   1.2.15 2026-08-29
#> 23     metalite.ae    0.1.4 2026-08-29
#> 24        plotthis   0.14.0 2026-08-29
#> 25           SLOPE    2.1.1 2026-08-29
#> 26          tgstat    2.4.0 2026-08-29
#> 27      GetFREData    1.1.0 2026-08-29
#> 28             PNC    0.2.0 2026-08-29
#> 29           FLSSS    9.3.0 2026-08-29
#> 30     SpaDES.core    3.2.1 2026-08-29
#> 31         UBStats    0.4.3 2026-08-29
#> 32       PLNmodels    1.3.1 2026-08-29
#> 33            mxcc    0.0.6 2026-08-29
#> 34          Rserve 1.8-19.1 2026-08-29
#> 35         iotools  0.4-0.1 2026-08-29
#> 36         Require    2.1.0 2026-08-29
#> 37    twoCoprimary    1.1.0 2026-08-29
```
