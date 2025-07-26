#' Get the bincraft logger
#'
#' @return lgr logger object for bincraft package
#' @keywords internal
get_logger <- function() {
  lgr::get_logger("bincraft")
}

#' Log a debug message with cli formatting
#'
#' @param msg The message to log
#' @param ... Additional arguments passed to cli functions
#' @keywords internal
log_debug <- function(msg, ...) {
  logger <- get_logger()
  if (logger$threshold >= 500L) { # DEBUG level
    cli::cli_inform(paste("Debug:", msg), ...)
  }
  logger$debug(msg)
}

#' Log an info message with cli formatting
#'
#' @param msg The message to log
#' @param ... Additional arguments passed to cli functions
#' @keywords internal
log_info <- function(msg, ...) {
  logger <- get_logger()
  if (logger$threshold >= 400L) { # INFO level
    cli::cli_alert_info(msg, ...)
  }
  logger$info(msg)
}

#' Log a success message with cli formatting
#'
#' @param msg The message to log
#' @param ... Additional arguments passed to cli functions
#' @keywords internal
log_success <- function(msg, ...) {
  logger <- get_logger()
  if (logger$threshold >= 400L) { # INFO level
    cli::cli_alert_success(msg, ...)
  }
  logger$info(msg)
}

#' Log a warning message with cli formatting
#'
#' @param msg The message to log
#' @param ... Additional arguments passed to cli functions
#' @keywords internal
log_warn <- function(msg, ...) {
  logger <- get_logger()
  if (logger$threshold >= 300L) { # WARN level
    cli::cli_alert_warning(msg, ...)
  }
  logger$warn(msg)
}

#' Log an error message with cli formatting
#'
#' @param msg The message to log
#' @param ... Additional arguments passed to cli functions
#' @keywords internal
log_error <- function(msg, ...) {
  logger <- get_logger()
  if (logger$threshold >= 200L) { # ERROR level
    cli::cli_alert_danger(msg, ...)
  }
  logger$error(msg)
}

#' Log a header message with cli formatting
#'
#' @param msg The message to log
#' @param ... Additional arguments passed to cli functions
#' @keywords internal
log_header <- function(msg, ...) {
  logger <- get_logger()
  if (logger$threshold >= 400L) { # INFO level
    cli::cli_h2(msg, ...)
  }
  logger$info(msg)
}
