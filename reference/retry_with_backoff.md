# Retry a function with exponential backoff

Retries a function with exponential backoff strategy, classifying errors
to determine if retry is appropriate. Only retries transient network/IO
errors, not dependency resolution or compile errors.

## Usage

``` r
retry_with_backoff(func, max_attempts = 10L, base_delay = 1L, max_delay = 60L)
```

## Arguments

- func:

  (function)  
  Function to retry (should be a zero-argument function)

- max_attempts:

  (integer)  
  Maximum number of attempts (default: 5L)

- base_delay:

  (numeric)  
  Base delay in seconds (default: 1)

- max_delay:

  (numeric)  
  Maximum delay in seconds (default: 60)

## Value

Result of successful function execution

## Details

Exponential backoff: delay = min(base_delay \* 2^(attempt-1), max_delay)
