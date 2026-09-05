# Returns R minor sensitive R packages

If `interval` is set, it checks for updates of these packages in the
CRAN repository and returns a filtered response.

## Usage

``` r
get_r_minor_sensitive_packages(
  r_minor_packages_forge_type = "Forgejo",
  r_minor_packages_issue_url = NULL,
  interval = NULL,
  updated_packages = NULL,
  new_packages = NULL
)
```

## Arguments

- r_minor_packages_forge_type:

  (character)  
  Forge type to query R minor sensitive packages from. Available options
  are "Forgejo" and "GitHub".

- r_minor_packages_issue_url:

  (character)  
  URL to issue containing R minor sensitive packages.

- interval:

  (`lubridate::Interval`)  
  Date interval

- updated_packages:

  (character)  
  Vector of updated packages CRAN to check against.

- new_packages:

  (character)  
  Vector of new packages on CRAN to check against.
