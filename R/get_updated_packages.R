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
  limit = 2000L
) {
  events <- tryCatch(
    purrr::insistently(
      ~ pkgsearch::cran_events(
        releases = TRUE,
        archivals = FALSE,
        limit = limit
      ),
      rate = retry_config,
      quiet = FALSE
    )(),
    error = function(e) {
      log_warn(sprintf(
        "{.fun get_updated_cran_packages}: pkgsearch::cran_events failed after retries: %s. Returning empty result.", # nolint
        conditionMessage(e)
      ))
      NULL
    }
  )

  if (is.null(events) || length(events) == 0L) {
    return(empty_cran_df())
  }

  events_filtered <- purrr::keep(
    events,
    ~ as.Date(.x$date) %within% date_interval
  )

  if (length(events_filtered) == 0L) {
    return(empty_cran_df())
  }

  events_formatted <- data.frame(
    name = purrr::map_chr(events_filtered, "name"),
    version = purrr::map_chr(events_filtered, ~ .x$package$Version),
    date = as.Date(purrr::map_chr(events_filtered, "date"))
  )

  # new pkgs
  new_pkgs_df <- get_new_cran_packages(date_interval)

  # remove new pkgs, keeping only updated ones
  events_formatted[!events_formatted$name %in% new_pkgs_df$name, ]
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
  )
) {
  new_packages <- tryCatch(
    purrr::insistently(
      ~ pkgsearch::cran_new(
        from = lubridate::int_start(date_interval),
        to = lubridate::int_end(date_interval)
      ),
      rate = retry_config,
      quiet = FALSE
    )(),
    error = function(e) {
      log_warn(sprintf(
        "{.fun get_new_cran_packages}: pkgsearch::cran_new failed after retries: %s. Returning empty result.", # nolint
        conditionMessage(e)
      ))
      NULL
    }
  )

  if (
    is.null(new_packages) ||
      nrow(new_packages) == 0L ||
      is.null(new_packages$Package)
  ) {
    return(empty_cran_df())
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
  limit = 300L
) {
  events <- tryCatch(
    purrr::insistently(
      ~ pkgsearch::cran_events(releases = FALSE, limit = limit),
      rate = retry_config,
      quiet = FALSE
    )(),
    error = function(e) {
      log_warn(sprintf(
        "{.fun get_removed_cran_packages}: pkgsearch::cran_events failed after retries: %s. Returning empty result.", # nolint
        conditionMessage(e)
      ))
      NULL
    }
  )

  if (is.null(events) || length(events) == 0L) {
    return(empty_cran_df())
  }

  events_filtered <- purrr::keep(
    events,
    ~ as.Date(.x$date) %within% date_interval
  )

  if (length(events_filtered) == 0L) {
    return(empty_cran_df())
  }

  data.frame(
    name = purrr::map_chr(events_filtered, "name"),
    version = purrr::map_chr(events_filtered, ~ .x$package$Version),
    date = as.Date(purrr::map_chr(events_filtered, "date"))
  )
}

# Canonical empty result shared by the three CRAN feed helpers.
empty_cran_df <- function() {
  data.frame(
    name = character(0L),
    version = character(0L),
    date = as.Date(character(0L)),
    stringsAsFactors = FALSE
  )
}
