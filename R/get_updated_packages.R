#' Get updated CRAN packages
#' @template param-date_interval
#' @template param-limit
#'
#' @importFrom purrr map list_rbind
#' @importFrom pkgsearch cran_new cran_events
#' @importFrom lubridate `%within%` int_start int_end interval today
#' @examples
#' get_updated_cran_packages()
#' @export
get_updated_cran_packages <- function(
    date_interval = lubridate::interval(
      lubridate::today() - 7L,
      lubridate::today()
    ),
    limit = 2000L) {
  events <- pkgsearch::cran_events(
    releases = TRUE,
    archivals = FALSE,
    limit = limit
  )
  events_filtered <- purrr::keep(
    events,
    ~ as.Date(.x$date) %within% date_interval
  )
  events_formatted <- data.frame(
    name = purrr::map_chr(events_filtered, "name"),
    version = purrr::map_chr(events_filtered, ~ .x$package$Version),
    date = as.Date(purrr::map_chr(events_filtered, "date"))
  )

  # new pkgs
  new_pkgs_df <- get_new_cran_packages(date_interval)

  # remove new pkgs, keeping only updated ones
  df_diff <- events_formatted[!events_formatted$name %in% new_pkgs_df$name, ]
  return(df_diff)
}

#' Get new CRAN packages
#' @template param-date_interval
#'
#' @importFrom purrr map list_rbind
#' @importFrom pkgsearch cran_new
#' @export
#' @examples
#' \dontrun{
#' # last week
#' get_new_cran_packages()
#' }
#'
get_new_cran_packages <- function(
    date_interval = lubridate::interval(
      lubridate::today() - 7L,
      lubridate::today()
    )) {
  new_packages <- pkgsearch::cran_new(
    from = lubridate::int_start(date_interval),
    to = lubridate::int_end(date_interval)
  )

  # Handle empty results
  if (nrow(new_packages) == 0L || is.null(new_packages$Package)) {
    return(data.frame(
      name = character(0L),
      version = character(0L),
      date = as.Date(character(0L)),
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    name = new_packages$Package,
    version = new_packages$Version,
    date = as.Date(new_packages$`Date/Publication`),
    stringsAsFactors = FALSE
  )
}

#' Get removed CRAN packages
#' @template param-date_interval
#' @template param-limit
#' @importFrom purrr map list_rbind
#' @importFrom pkgsearch cran_events
#' @export
#' @examples
#' \dontrun{
#' get_removed_cran_packages()
#' }
#'
get_removed_cran_packages <- function(
    date_interval = lubridate::interval(
      lubridate::today() - 7L,
      lubridate::today()
    ),
    limit = 300L) {
  events <- pkgsearch::cran_events(releases = FALSE, limit = limit)

  events_filtered <- purrr::keep(
    events,
    ~ as.Date(.x$date) %within% date_interval
  )

  data.frame(
    name = purrr::map_chr(events_filtered, "name"),
    version = purrr::map_chr(events_filtered, ~ .x$package$Version),
    date = as.Date(purrr::map_chr(events_filtered, "date"))
  )
}
