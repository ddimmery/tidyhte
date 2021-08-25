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