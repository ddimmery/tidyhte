#' @export
fit.plugin <- function(.data, outcome_col, ..., .Model_cfg) {
    dots <- rlang::enexprs(...)
    predictor <- predictor_factory(.Model_cfg)

    data <- Model.data$new(.data, {{ outcome_col }}, !!!dots)

    predictor$fit(data)
}


#' @export
fit.plugin.A <- function(.data, a_col, ..., .Model_cfg) {
    dots <- rlang::enexprs(...)
    if (.Model_cfg$model_class == "known") {
        cov <- rlang::sym(.Model_cfg$covariate_name)
        dots <- unique(c(dots, cov))
    }

    list(
        pi = fit.plugin(.data, {{ a_col }}, !!!dots, .Model_cfg = .Model_cfg)
    )
}

#' @export
fit.plugin.Y <- function(.data, y_col, a_col, ..., .Model_cfg) {
    dots <- rlang::enexprs(...)

    df_0 <- dplyr::filter(.data, {{ a_col }} == 0)
    df_1 <- dplyr::filter(.data, {{ a_col }} == 1)

    list(
        mu1 = fit.plugin(df_1, {{ y_col }}, !!!dots, .Model_cfg = .Model_cfg),
        mu0 = fit.plugin(df_0, {{ y_col }}, !!!dots, .Model_cfg = .Model_cfg)
    )
}

#' @export
fit.effect <- function(.data, fx_col, ..., .Model_cfg) {
    dots <- rlang::enexprs(...)

    list(
        fx = fit.plugin(.data, {{ fx_col }}, !!!dots, .Model_cfg = .Model_cfg)
    )
}