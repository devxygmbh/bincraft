utils::globalVariables(c(".", "OS_type", "Package", "%nin%"))

#' Not in operator
#'
#' Returns TRUE for elements not in a set.
#' @param x Vector or NULL: the values to be matched
#' @param table Vector or NULL: the values to be matched against
#' @return A logical vector, indicating if each element of x is NOT in table
#' @usage x \%nin\% table
#' @keywords internal
#' @export
`%nin%` <- function(x, table) match(x, table, nomatch = 0L) <= 0L

.onLoad <- function(libname, pkgname) {
  # Get the log level from environment variable or default to INFO
  log_level <- Sys.getenv("BINCRAFT_LOG_LEVEL", "info")

  # Validate log level and use default if invalid
  valid_levels <- c("fatal", "error", "warn", "info", "debug", "trace")
  if (!tolower(log_level) %in% valid_levels) {
    log_level <- "info"
  }

  # Create a file appender for internal logging (no console output)
  # Only write to file/memory for debugging, not to console
  memory_appender <- lgr::AppenderBuffer$new(
    threshold = log_level,
    layout = lgr::LayoutFormat$new(
      fmt = "[{timestamp}] {level}: {msg}",
      timestamp_fmt = "%Y-%m-%d %H:%M:%S"
    )
  )

  # Create package-specific logger that only logs internally
  pkg_logger <- lgr::get_logger("bincraft")
  pkg_logger$
    set_threshold(log_level)$
    set_appenders(list(memory = memory_appender))$
    set_propagate(FALSE)
}
