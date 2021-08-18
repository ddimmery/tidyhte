#' @export
fit.plugin <- function(.data, a_col, ..., .Model.cfg) {
    dots <- rlang::enexprs(...)
    predictor <- predictor_factory(.Model.cfg)

    data <- Model.data$new(.data, {{ a_col }}, !!!dots)

    predictor$fit(data)
}


#' @export
fit.plugin.A <- function(.data, a_col, ..., .Model.cfg) {
    dots <- rlang::enexprs(...)
    if (.Model.cfg$model_class == "known") {
        cov <- rlang::sym(.Model.cfg$covariate_name)
        dots <- unique(c(dots, cov))
    }

    list(
        pi = fit.plugin(.data, {{ a_col }}, !!!dots, .Model.cfg = .Model.cfg)
    )
}

#' @export
fit.plugin.Y <- function(.data, y_col, a_col, ..., .Model.cfg) {
    dots <- rlang::enexprs(...)

    df_0 <- dplyr::filter(.data, {{ a_col }} == 0)
    df_1 <- dplyr::filter(.data, {{ a_col }} == 1)

    list(
        mu1 = fit.plugin(df_1, {{ y_col }}, !!!dots, .Model.cfg = .Model.cfg),
        mu0 = fit.plugin(df_0, {{ y_col }}, !!!dots, .Model.cfg = .Model.cfg)
    )
}

#' @export
fit.effect <- function(.data, fx_col, ..., .Model.cfg) {
    dots <- rlang::enexprs(...)

    list(
        fx = fit.plugin(df_1, {{ fx_col }}, !!!dots, .Model.cfg = .Model.cfg)
    )
}