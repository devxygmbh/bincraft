test_that("get_new_cran_packages returns correct structure", {
  date_interval <- lubridate::interval(
    lubridate::today() - 7,
    lubridate::today()
  )
  
  result <- get_new_cran_packages(date_interval)
  
  expect_s3_class(result, "data.frame")
  expect_named(result, c("name", "version", "date"))
  
  # If there are results, check column types and date range
  if (nrow(result) > 0) {
    expect_type(result$name, "character")
    expect_type(result$version, "character")
    expect_s3_class(result$date, "Date")
    
    # Check that dates are within the specified interval
    expect_true(all(result$date >= lubridate::int_start(date_interval)))
    expect_true(all(result$date <= lubridate::int_end(date_interval)))
  }
})

test_that("get_new_cran_packages handles empty results", {
  # Use a very old date range that should return no results
  date_interval <- lubridate::interval(
    as.Date("1970-01-01"),
    as.Date("1970-01-02")
  )
  
  result <- get_new_cran_packages(date_interval)
  
  # Should still be a data frame with correct structure
  expect_s3_class(result, "data.frame")
  expect_named(result, c("name", "version", "date"))
  expect_equal(nrow(result), 0)
})

test_that("get_removed_cran_packages returns correct structure", {
  result <- get_removed_cran_packages(limit = 10)
  
  expect_s3_class(result, "data.frame")
  
  expect_named(result, c("name", "date"))
  
  expect_type(result$name, "character")
  expect_s3_class(result$date, "Date")
  
  expect_lte(nrow(result), 10)
})

test_that("get_removed_cran_packages respects limit parameter", {
  # Test with different limits
  result_5 <- get_removed_cran_packages(limit = 5)
  result_20 <- get_removed_cran_packages(limit = 20)
  
  expect_lte(nrow(result_5), 5)
  expect_lte(nrow(result_20), 20)
})

test_that("get_updated_cran_packages returns correct structure", {
  # Test with a specific date range
  date_interval <- lubridate::interval(
    lubridate::today() - 30,
    lubridate::today()
  )
  
  result <- get_updated_cran_packages(date_interval, limit = 100)
  
  # Check that result is a data frame
  expect_s3_class(result, "data.frame")
  
  # Check column names
  expect_named(result, c("name", "version", "date"))
  
  # Check column types
  expect_type(result$name, "character")
  expect_type(result$version, "character")
  expect_s3_class(result$date, "Date")
  
  # Check that dates are within the specified interval
  if (nrow(result) > 0) {
    expect_true(all(result$date >= lubridate::int_start(date_interval)))
    expect_true(all(result$date <= lubridate::int_end(date_interval)))
  }
})

test_that("get_updated_cran_packages excludes new packages", {
  # Use a date range that likely has both new and updated packages
  date_interval <- lubridate::interval(
    lubridate::today() - 7,
    lubridate::today()
  )
  
  # Get all three types of packages
  updated_pkgs <- get_updated_cran_packages(date_interval, limit = 500)
  new_pkgs <- get_new_cran_packages(date_interval)
  
  # Check that no new packages appear in the updated packages list
  if (nrow(new_pkgs) > 0 && nrow(updated_pkgs) > 0) {
    expect_false(any(updated_pkgs$name %in% new_pkgs$name))
  }
})

test_that("get_updated_cran_packages handles empty results gracefully", {
  # Use a very old date range
  date_interval <- lubridate::interval(
    as.Date("1970-01-01"),
    as.Date("1970-01-02")
  )
  
  result <- get_updated_cran_packages(date_interval)
  
  # Should still be a data frame with correct structure
  expect_s3_class(result, "data.frame")
  expect_named(result, c("name", "version", "date"))
})

test_that("default date intervals work correctly", {
  # Test that default parameters don't cause errors
  expect_no_error({
    new_pkgs <- get_new_cran_packages()
    updated_pkgs <- get_updated_cran_packages()
  })
})

test_that("date filtering works correctly in get_updated_cran_packages", {
  # Test with a known date range
  start_date <- lubridate::today() - 14
  end_date <- lubridate::today() - 7
  date_interval <- lubridate::interval(start_date, end_date)
  
  result <- get_updated_cran_packages(date_interval, limit = 1000)
  
  # All dates should be within the interval
  if (nrow(result) > 0) {
    expect_true(all(result$date >= start_date))
    expect_true(all(result$date <= end_date))
  }
})

# Mock tests for error handling and edge cases
test_that("functions handle API errors gracefully", {
  # Mock pkgsearch functions to simulate API errors
  skip_if_not_installed("mockery")
  
  # Test get_new_cran_packages with API error
  mockery::stub(get_new_cran_packages, "pkgsearch::cran_new", 
                function(...) stop("API error"))
  expect_error(get_new_cran_packages())
  
  # Test get_removed_cran_packages with API error
  mockery::stub(get_removed_cran_packages, "pkgsearch::cran_events", 
                function(...) stop("API error"))
  expect_error(get_removed_cran_packages())
  
  # Test get_updated_cran_packages with API error
  mockery::stub(get_updated_cran_packages, "pkgsearch::cran_events", 
                function(...) stop("API error"))
  expect_error(get_updated_cran_packages())
})

# Integration test
test_that("all three functions work together consistently", {
  # Use the same date interval for all three
  date_interval <- lubridate::interval(
    lubridate::today() - 3,
    lubridate::today()
  )
  
  new_pkgs <- get_new_cran_packages(date_interval)
  updated_pkgs <- get_updated_cran_packages(date_interval, limit = 500)
  removed_pkgs <- get_removed_cran_packages(limit = 100)
  
  # All should return data frames
  expect_s3_class(new_pkgs, "data.frame")
  expect_s3_class(updated_pkgs, "data.frame")
  expect_s3_class(removed_pkgs, "data.frame")
  
  # New and updated packages should be mutually exclusive
  if (nrow(new_pkgs) > 0 && nrow(updated_pkgs) > 0) {
    common_pkgs <- intersect(new_pkgs$name, updated_pkgs$name)
    expect_equal(length(common_pkgs), 0)
  }
})