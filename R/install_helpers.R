#' Classify a uvr failure for retry decisions
#'
#' Retries transient network/IO failures; fails fast on dependency resolution
#' and compile errors, which are deterministic and would burn every retry
#' attempt for nothing. A message that mentions both a transient and a
#' deterministic cause is treated as deterministic (fail fast).
#'
#' @template param-error_msg
#' @return List with should_retry (logical) and error_type (character)
classify_uvr_error <- function(error_msg) {
  is_resolution_error <- grepl(
    paste0(
      "could not resolve|no (matching |suitable )?version|",
      "unsatisfiable|cannot find package|unknown package|",
      "not available|dependency conflict"
    ),
    error_msg,
    ignore.case = TRUE
  )

  # A deterministic compile/build failure (a missing header, a failed make, a
  # package whose source will not compile) fails identically on every retry, so
  # treat it as non-retryable and let the build fail fast.
  is_compile_error <- grepl(
    paste0(
      "compilation failed for package|",
      "fatal error:|", # C/C++ preprocessor/compile errors
      "make(\\[[0-9]+\\])?: \\*\\*\\*|", # a failed make rule
      "ERROR: (compilation|configuration) failed"
    ),
    error_msg,
    ignore.case = TRUE
  )

  is_network_error <- grepl(
    paste0(
      "error sending request|connection (reset|refused|closed)|",
      "timed out|timeout|network|temporarily unavailable|",
      "failed to (download|fetch)|HTTP (5[0-9][0-9]|429)|dns error"
    ),
    error_msg,
    ignore.case = TRUE
  )

  should_retry <- is_network_error &&
    !is_resolution_error &&
    !is_compile_error

  error_type <- if (is_compile_error) {
    "compile error"
  } else if (is_resolution_error) {
    "resolution error"
  } else if (is_network_error) {
    "network error"
  } else {
    "uvr error"
  }

  list(should_retry = should_retry, error_type = error_type)
}

#' Clone package repository
#'
#' Clones a package repository if it doesn't already exist locally.
#'
#' @template param-package_name
#' @template param-tag
#' @template param-local_clone_dir_single
#' @return Invisible NULL
clone_package_repo <- function(package_name, tag, local_clone_dir_single) {
  log_debug(
    sprintf(
      "Cloning package {.pkg %s} with tag {.field %s}.",
      package_name[1L],
      tail(tag, 1L)
    )
  )

  if (!dir.exists(local_clone_dir_single)) {
    system2(
      "git",
      args = c(
        "clone",
        "-q",
        sprintf("--branch=%s", tail(tag, 1L)),
        sprintf("https://github.com/cran/%s", package_name[1L]),
        local_clone_dir_single
      )
    )
  }
}

#' Retry a function with exponential backoff
#'
#' Retries a function with exponential backoff strategy, classifying errors
#' to determine if retry is appropriate. Only retries transient network/IO
#' errors, not dependency resolution or compile errors.
#'
#' @template param-func
#' @template param-max_attempts
#' @template param-base_delay
#' @template param-max_delay
#' @return Result of successful function execution
#' @details Exponential backoff: delay = min(base_delay * 2^(attempt-1), max_delay)
retry_with_backoff <- function(
  func,
  max_attempts = 10L,
  base_delay = 1L,
  max_delay = 60L
) {
  for (attempt in seq_len(max_attempts)) {
    tryCatch(
      {
        return(func())
      },
      error = function(e) {
        error_msg <- conditionMessage(e)
        error_classification <- classify_uvr_error(error_msg)

        if (!error_classification$should_retry) {
          # Resolution and compile errors are deterministic; fail immediately.
          if (error_classification$error_type == "resolution error") {
            log_error(
              sprintf(
                "Dependency resolution error (not retryable): %s",
                error_msg
              )
            )
          }
          # Escape braces in error message to prevent cli/glue interpretation
          stop(
            gsub(
              "}",
              "}}",
              gsub("{", "{{", error_msg, fixed = TRUE),
              fixed = TRUE
            ),
            call. = FALSE
          )
        }

        if (attempt >= max_attempts) {
          log_error(
            sprintf("All %s attempts failed. Giving up.", max_attempts)
          )
          # Escape braces in error message to prevent cli/glue interpretation
          stop(
            gsub(
              "}",
              "}}",
              gsub("{", "{{", error_msg, fixed = TRUE),
              fixed = TRUE
            ),
            call. = FALSE
          )
        }

        delay <- min(base_delay * (2L^(attempt - 1L)), max_delay)
        log_warn(sprintf(
          "Attempt %d/%d failed with %s. Retrying in %g seconds...",
          attempt,
          max_attempts,
          error_classification$error_type,
          delay
        ))

        Sys.sleep(delay)
      }
    )
  }
}
