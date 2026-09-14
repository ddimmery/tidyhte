#' @noRd
#' @keywords internal
muffle_warnings <- function(expr, ...) {
  regex <- paste(..., sep = "|")
  withCallingHandlers(
    expr,
    warning = function(w) {
      if (grepl(regex, conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

#' @noRd
#' @keywords internal
muffle_messages <- function(expr, ...) {
  regex <- paste(..., sep = "|")
  withCallingHandlers(
    expr,
    message = function(m) {
      if (grepl(regex, conditionMessage(m))) {
        invokeRestart("muffleMessage")
      }
    }
  )
}

#' @noRd
#' @keywords internal
#' @importFrom rlang check_installed
soft_require <- function(package, load = FALSE, reason = NULL) {
  rlang::check_installed(package, reason = reason)
  if (load) {
    try(attachNamespace(package), silent = TRUE)
  }
}

#' @noRd
#' @keywords internal
package_present <- function(package) {
  rlang::is_installed(package)
}

#' Warn that SuperLearner is unavailable and base-R GLMs will be used instead.
#' @noRd
#' @keywords internal
warn_sl_fallback <- function() {
  rlang::warn(
    c(
      "`SuperLearner` is not installed, so nuisance models will be fit with simple base-R GLMs.",
      "!" = paste(
        "These are single generalized linear models, not flexible SuperLearner ensembles.",
        "Estimates may be biased if the true nuisance functions are non-linear."
      ),
      "i" = "Run `install.packages(\"SuperLearner\")` to enable ensembles."
    ),
    class = "tidyhte_warning_sl_fallback"
  )
}

#' Warn whenever nuisance models are about to be fit with a `GLM_cfg`.
#' @param ... Named `Model_cfg` objects (names are used in the warning text).
#' @noRd
#' @keywords internal
warn_glm_models <- function(...) {
  cfgs <- rlang::list2(...)
  is_glm <- vapply(
    cfgs,
    function(cfg) isTRUE(tryCatch(cfg$model_class == "GLM", error = function(e) FALSE)),
    logical(1)
  )
  if (!any(is_glm)) return(invisible(NULL))
  which_models <- paste0("`", names(cfgs)[is_glm], "`", collapse = ", ")
  rlang::warn(
    c(
      paste0("Fitting ", which_models, " model(s) with a simple GLM (`GLM_cfg`)."),
      "!" = paste(
        "This is a single generalized linear model, not a flexible ensemble;",
        "results rely on the GLM being correctly specified."
      ),
      "i" = "Add learners with `add_*_model(\"SL.*\")` to use a SuperLearner ensemble instead."
    ),
    class = "tidyhte_warning_glm_model"
  )
  invisible(NULL)
}

#' @noRd
#' @keywords internal
check_hte_cfg <- function(cfg) {
  checkmate::check_r6(cfg, classes = "HTE_cfg")
}

#' @noRd
#' @keywords internal
zero_range <- function(x, tol = .Machine$double.eps ^ 0.5) {
  if (length(x) == 1) return(TRUE)
  x <- range(x) / mean(x)
  isTRUE(all.equal(x[1], x[2], tolerance = tol))
}

#' @noRd
#' @keywords internal
#' @importFrom stats weighted.mean
clustered_se_of_mean <- function(y, cluster, weights = rep(1, length(y))) {
  n <- length(y)
  weights <- weights / sum(weights) * n
  H <- length(unique(cluster))
  yhat <- stats::weighted.mean(y, weights)
  if (H < n) {
    dplyr::tibble(r = y - yhat, w = weights, cl = cluster) %>%
      dplyr::group_by(.data$cl) %>%
      dplyr::summarize(r = sum(tcrossprod(.data$w) * tcrossprod(.data$r))) %>%
      dplyr::select("r") %>%
      unlist() -> cl_resids
  } else {
    cl_resids <- weights ^ 2 * (y - yhat) ^ 2
  }
  sqrt(sum(cl_resids) / n ^ 2 * H / (H - 1))
}
