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

soft_require <- function(package, load = FALSE) {
    if (load) {
        is_ok <- requireNamespace(package, quietly = FALSE)
        if (is_ok) try(attachNamespace(package), silent = TRUE)
    } else {
        is_ok <- package_present(package)
    }
    if (!is_ok) {
        stop(paste("loading required package (", package, ") failed", sep = ""), call. = FALSE)
    }
}

package_present <- function(package) {
    path <- find.package(package, quiet = TRUE)
    length(path) > 0
}

clustered_se_of_mean <- function(y, cluster, yhat = mean(y)) {
    n <- length(y)
    H <- length(unique(cluster))
    sqrt(sum(tapply(y - yhat, cluster, sum)^2) / n ^ 2 * H / (H - 1))
}
