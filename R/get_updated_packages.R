#' Get updated CRAN packages
#' @template param-date
#'
#' @importFrom purrr map list_rbind
#' @importFrom lubridate today
#' @examples
#' get_updated_cran_packages(lubridate::interval(lubridate::today() - 7, lubridate::today()))
#' @export
get_updated_cran_packages <- function(date = lubridate::today()) {
  feed <- get_cranberries_feed()

  # Apply the function to each item and bind the results into a data frame
  results <- purrr::map(feed, process_cranberries_rss, date) |>
    purrr::list_rbind()

  return(results)
}

#' Get new CRAN packages
#' @template param-date
#'
#' @importFrom purrr map list_rbind
#' @importFrom lubridate today
#' @export
#' @examples
#' \dontrun{
#' # last week
#' get_new_cran_packages(lubridate::interval(lubridate::today() - 7, lubridate::today()))
#' }
#'
get_new_cran_packages <- function(date = lubridate::today()) {
  feed <- get_cranberries_feed(type = "new")

  # Apply the function to each item and bind the results into a data frame
  results <- purrr::map(feed, process_cranberries_rss, date) |>
    purrr::list_rbind()

  return(results)
}

#' Get removed CRAN packages
#' @template param-date
#'
#' @importFrom purrr map list_rbind
#' @importFrom lubridate today
#' @export
#' @examples
#' \dontrun{
#' get_removed_cran_packages()
#' get_removed_cran_packages(lubridate::interval(today() - 7, today()))
#' }
#'
get_removed_cran_packages <- function(date = lubridate::today()) {
  feed <- get_cranberries_feed(type = "removed")

  # Apply the function to each item and bind the results into a data frame
  results <- purrr::map(feed, process_cranberries_rss, date) |>
    purrr::list_rbind()

  if (nrow(results == 0L)) {
    cli::cli_alert_info("{.fun archive_package}: No packages to archive for interval {.field {date}}.")
  }

  return(results)
}

#' Get Cranberries fieed
#' @importFrom xml2 read_xml xml_find_all
#' @export
#' @param type Which type of packages to query. Allowed are `"updated"`, `"new"` and `"removed"`
get_cranberries_feed <- function(type = "updated") {
  feed <- sprintf("https://dirk.eddelbuettel.com/cranberries/cran/%s/index.rss", type)

  # Fetch and parse the RSS feed
  rss_content <- xml2::read_xml(feed)

  # Extract item nodes
  items <- xml2::xml_find_all(rss_content, "//item")

  return(items)
}

#' Process CRANberries RSS feed
#' @template param-date
#' @param feed RSS feed
#'
#' @importFrom lubridate dmy_hms as_date interval %within%
#' @importFrom xml2 xml_text xml_find_first
#' @export
process_cranberries_rss <- function(feed, date = lubridate::today()) {
  pkg_title <- xml2::xml_text(xml_find_first(feed, "title"))
  pub_date_text <- xml2::xml_text(xml_find_first(feed, "pubDate"))
  pub_date <- lubridate::as_date(dmy_hms(pub_date_text))
  if (inherits(date, "Date")) {
    date <- lubridate::interval(date, date)
  }

  if (pub_date %within% date) {
    if (grepl("updated", pkg_title, fixed = TRUE)) {
      package_info <- strsplit(pkg_title, " ", fixed = TRUE)[[1L]]
      package_name <- package_info[2L]
      package_version_new <- package_info[7L]
      package_version_old <- package_info[12L]
      previous_update_date <- package_info[14L]

      return(data.frame(
        name = package_name,
        version = package_version_new,
        date = pub_date,
        version_old = package_version_old,
        previous_update_date = previous_update_date
      ))
    } else if (grepl("New package", pkg_title, fixed = TRUE)) {
      package_info <- strsplit(pkg_title, " ", fixed = TRUE)[[1L]]
      package_name <- package_info[3L]
      package_version_new <- package_info[7L]

      return(data.frame(
        name = package_name,
        version = package_version_new,
        date = pub_date
      ))
    } else if (grepl("was removed", pkg_title, fixed = TRUE)) {
      package_info <- strsplit(pkg_title, " ", fixed = TRUE)[[1L]]
      package_name <- package_info[2L]

      return(data.frame(
        name = package_name,
        date = pub_date
      ))
    }
  } else {
    NULL
  }
}
