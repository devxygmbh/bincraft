#' Get the bincraft logger
#'
#' @return lgr logger object for bincraft package
#' @keywords internal
get_logger <- function() {
  lgr::get_logger("bincraft")
}

#' Safely escape curly braces for cli while preserving intentional formatting
#'
#' @param msg The message to process
#' @return Message with unmatched braces escaped
#' @keywords internal
safe_cli_msg <- function(msg) {
  # Preserve valid cli patterns like {.pkg %s}, {.field %s}, etc.
  # These are intentional cli formatting that should be preserved
  cli_patterns <- c(
    "\\{\\.pkg [^}]+\\}",
    "\\{\\.field [^}]+\\}",
    "\\{\\.fun [^}]+\\}",
    "\\{\\.path [^}]+\\}",
    "\\{\\.val [^}]+\\}",
    "\\{\\.arg [^}]+\\}",
    "\\{\\.code [^}]+\\}",
    "\\{\\.file [^}]+\\}",
    "\\{\\.var [^}]+\\}"
  )

  # Find all valid cli patterns and mark them
  protected_patterns <- list()
  for (i in seq_along(cli_patterns)) {
    matches <- gregexpr(cli_patterns[i], msg, perl = TRUE)[[1L]]
    if (matches[1L] != -1L) {
      for (j in seq_along(matches)) {
        start_pos <- matches[j]
        length_match <- attr(matches, "match.length")[j]
        pattern_text <- substr(msg, start_pos, start_pos + length_match - 1L)
        placeholder <- paste0("__CLI_PATTERN_", i, "_", j, "__")
        protected_patterns[[placeholder]] <- pattern_text
        msg <- sub(pattern_text, placeholder, msg, fixed = TRUE)
      }
    }
  }

  # Now escape any remaining unmatched braces
  msg <- gsub("\\{", "{{", msg, fixed = TRUE)
  msg <- gsub("\\}", "}}", msg, fixed = TRUE)

  # Restore protected patterns
  for (placeholder in names(protected_patterns)) {
    msg <- sub(placeholder, protected_patterns[[placeholder]], msg, fixed = TRUE)
  }

  msg
}

#' Log a debug message with cli formatting
#'
#' @param msg The message to log
#' @param ... Additional arguments passed to cli functions
#' @keywords internal
log_debug <- function(msg, ...) {
  logger <- get_logger()
  if (logger$threshold >= 500L) { # DEBUG level
    safe_msg <- safe_cli_msg(msg)
    cli::cli_inform(paste("Debug:", safe_msg))
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
    safe_msg <- safe_cli_msg(msg)
    cli::cli_alert_info(safe_msg)
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
    safe_msg <- safe_cli_msg(msg)
    cli::cli_alert_success(safe_msg)
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
    safe_msg <- safe_cli_msg(msg)
    cli::cli_alert_warning(safe_msg)
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
    safe_msg <- safe_cli_msg(msg)
    cli::cli_alert_danger(safe_msg)
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
    safe_msg <- safe_cli_msg(msg)
    cli::cli_h2(safe_msg)
  }
  logger$info(msg)
}
