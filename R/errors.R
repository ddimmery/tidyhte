#' Error utilities for tidyhte package
#'
#' These functions provide a standardized way to throw classed errors throughout
#' the tidyhte package, replacing basic `stop()` calls with structured error
#' conditions that can be caught and handled programmatically.
#'
#' @details
#' The tidyhte package uses a hierarchical error class system based on rlang's
#' structured condition handling. All tidyhte errors inherit from the base class
#' `tidyhte_error_general` and include more specific subclasses:
#'
#' * `tidyhte_error_config` - Configuration-related errors
#' * `tidyhte_error_model` - Model-related errors  
#' * `tidyhte_error_data` - Data validation errors
#' * `tidyhte_error_not_implemented` - Not implemented functionality
#' * `tidyhte_error_package` - Package dependency errors
#'
#' These classed errors can be caught using `tryCatch()` or `rlang::catch_cnd()`
#' with the specific error class for more precise error handling.
#'
#' @examples
#' \dontrun{
#' # Catching specific error types
#' tryCatch(
#'   some_tidyhte_function(),
#'   tidyhte_error_config = function(e) cat("Configuration error:", conditionMessage(e)),
#'   tidyhte_error_data = function(e) cat("Data error:", conditionMessage(e))
#' )
#' }
#'
#' @name tidyhte-errors
#' @keywords internal
NULL

#' Throw a tidyhte error
#'
#' @param message Error message
#' @param class Error class suffix (will be prefixed with "tidyhte_error_")
#' @param ... Additional arguments passed to `rlang::abort()`
#' @keywords internal
tidyhte_abort <- function(message, class = "general", ...) {
    rlang::abort(
        message = message,
        class = paste0("tidyhte_error_", class),
        ...
    )
}

#' Configuration-related errors
#'
#' @param message Error message
#' @param ... Additional arguments passed to `tidyhte_abort()`
#' @keywords internal
abort_config <- function(message, ...) {
    tidyhte_abort(message, class = "config", ...)
}

#' Model-related errors
#'
#' @param message Error message
#' @param ... Additional arguments passed to `tidyhte_abort()`
#' @keywords internal
abort_model <- function(message, ...) {
    tidyhte_abort(message, class = "model", ...)
}

#' Data validation errors
#'
#' @param message Error message
#' @param ... Additional arguments passed to `tidyhte_abort()`
#' @keywords internal
abort_data <- function(message, ...) {
    tidyhte_abort(message, class = "data", ...)
}

#' Not implemented errors
#'
#' @param message Error message
#' @param ... Additional arguments passed to `tidyhte_abort()`
#' @keywords internal
abort_not_implemented <- function(message = "Not implemented", ...) {
    tidyhte_abort(message, class = "not_implemented", ...)
}

#' Package dependency errors
#'
#' @param message Error message
#' @param ... Additional arguments passed to `tidyhte_abort()`
#' @keywords internal
abort_package <- function(message, ...) {
    tidyhte_abort(message, class = "package", ...)
}