
#' @export
estimate.pseudo.outcomes <- function(YA, A, mu0, mu1, ps) {
    muA = A * mu1 + (1 - A) * mu0
    (A - ps) / (ps * (1 - ps)) (YA - muA) + mu1 - mu0
}