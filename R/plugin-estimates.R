fit_plugin <- function(.data, outcome_col, ..., .Model_cfg) {
    dots <- rlang::enexprs(...)
    predictor <- predictor_factory(.Model_cfg)

    data <- Model_data$new(.data, {{ outcome_col }}, !!!dots)

    predictor$fit(data)
}


fit_plugin_A <- function(.data, a_col, ..., .Model_cfg) {
    dots <- rlang::enexprs(...)
    if (.Model_cfg$model_class == "known") {
        cov <- rlang::sym(.Model_cfg$covariate_name)
        dots <- unique(c(dots, cov))
    }

    list(
        pi = fit_plugin(.data, {{ a_col }}, !!!dots, .Model_cfg = .Model_cfg)
    )
}


fit_plugin_Y <- function(.data, y_col, a_col, ..., .Model_cfg) {
    dots <- rlang::enexprs(...)

    df_0 <- dplyr::filter(.data, {{ a_col }} == 0)
    df_1 <- dplyr::filter(.data, {{ a_col }} == 1)

    list(
        mu1 = fit_plugin(df_1, {{ y_col }}, !!!dots, .Model_cfg = .Model_cfg),
        mu0 = fit_plugin(df_0, {{ y_col }}, !!!dots, .Model_cfg = .Model_cfg)
    )
}


fit_effect <- function(.data, fx_col, ..., .Model_cfg) {
    dots <- rlang::enexprs(...)

    list(
        fx = fit_plugin(.data, {{ fx_col }}, !!!dots, .Model_cfg = .Model_cfg)
    )
}
