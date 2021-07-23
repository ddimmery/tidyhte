
#' @export
fit.plugin.A <- function(.data, a_col, ..., .Model.cfg) {
    dots <- rlang::enexprs(...)
    if (.Model.cfg$model_class == "known") {
        cov <- rlang::sym(.Model.cfg$covariate_name)
        dots <- c(dots, cov)
    }
    predictor <- predictor_factory(.Model.cfg)

    data <- Model.data$new(.data, {{ a_col }}, !!!dots)

    list(
        pi = predictor$fit(data)
    )
}

#' @export
fit.plugin.Y <- function(.data, y_col, a_col, ..., .Model.cfg) {
    dots <- rlang::enexprs(...)

    df_0 <- dplyr::filter(.data, {{ a_col }} == 0)
    df_1 <- dplyr::filter(.data, {{ a_col }} == 1)

    data0 <- Model.data$new(df_0, {{ y_col }}, !!!dots)
    data1 <- Model.data$new(df_1, {{ y_col }}, !!!dots)

    mod1 <- predictor_factory(.Model.cfg)
    mod0 <- predictor_factory(.Model.cfg)

    list(
        mu1 = mod1$fit(data0),
        mu0 = mod0$fit(data1)
    )
}