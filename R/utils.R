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
soft_require <- function(package, load = FALSE) {
    rlang::check_installed(package)
    if (load) {
        try(attachNamespace(package), silent = TRUE)
    }
}

#' @noRd
#' @keywords internal
package_present <- function(package) {
    rlang::is_installed(package)
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
