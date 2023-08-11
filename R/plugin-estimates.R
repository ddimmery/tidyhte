#' Fits a plugin model using the appropriate settings
#'
#' This function prepares data, fits the appropriate models and returns the
#' resulting estimates in a standardized format.
#' @param full_data The full dataset of interest for the modelling problem.
#' @param weight_col The unquoted weighting variable name to use in model fitting.
#' @param outcome_col The unquoted column name to use as a label for the supervised
#' learning problem.
#' @param ... The unquoted names of covariates to use in the model.
#' @param .Model_cfg A `Model_cfg` object configuring the appropriate model type to use.
#' @return A new `Predictor` object of the appropriate subclass corresponding to the
#' `Model_cfg` fit to the data.
#' @keywords internal
fit_plugin <- function(full_data, weight_col, outcome_col, ..., .Model_cfg) {
    dots <- rlang::enexprs(...)
    predictor <- predictor_factory(.Model_cfg)

    data <- Model_data$new(full_data, {{ outcome_col }}, !!!dots, .weight_col = {{ weight_col }})

    muffle_warnings(predictor$fit(data), "rank-deficient fit", "grouped=FALSE")
}

#' Fits a propensity score model using the appropriate settings
#'
#' This function prepares data, fits the appropriate model and returns the
#' resulting estimates in a standardized format.
#' @param full_data The full dataset of interest for the modelling problem.
#' @param weight_col The unquoted weighting variable name to use in model fitting.
#' @param a_col The unquoted column name of the treatment.
#' @param ... The unquoted names of covariates to use in the model.
#' @param .Model_cfg A `Model_cfg` object configuring the appropriate model type to use.
#' @return A list with one element, `ps`. This element contains a `Predictor` object of
#' the appropriate subclass corresponding to the `Model_cfg` fit to the data.
#' @keywords internal
fit_plugin_A <- function(full_data, weight_col, a_col, ..., .Model_cfg) {
    dots <- rlang::enexprs(...)
    if (.Model_cfg$model_class == "known") {
        cov <- rlang::sym(.Model_cfg$covariate_name)
        dots <- unique(c(dots, cov))
    }

    list(
        pi = fit_plugin(full_data, {{ weight_col }}, {{ a_col }}, !!!dots, .Model_cfg = .Model_cfg)
    )
}


#' Fits a T-learner using the appropriate settings
#'
#' This function prepares data, fits the appropriate model and returns the
#' resulting estimates in a standardized format.
#' @param full_data The full dataset of interest for the modelling problem.
#' @param weight_col The unquoted weighting variable name to use in model fitting.
#' @param y_col The unquoted column name of the outcome.
#' @param a_col The unquoted column name of the treatment.
#' @param ... The unquoted names of covariates to use in the model.
#' @param .Model_cfg A `Model_cfg` object configuring the appropriate model type to use.
#' @return A list with two elements, `mu1` and `mu0` corresponding to the models fit to
#' the treatment and control potential outcomes, respectively. Each is a new `Predictor`
#' object of the appropriate subclass corresponding to the the `Model_cfg` fit to the data.
#' @keywords internal
fit_plugin_Y <- function(full_data, weight_col, y_col, a_col, ..., .Model_cfg) {
    dots <- rlang::enexprs(...)

    df_0 <- dplyr::filter(full_data, {{ a_col }} == 0)
    df_1 <- dplyr::filter(full_data, {{ a_col }} == 1)

    list(
        mu1 = fit_plugin(df_1, {{ weight_col }}, {{ y_col }}, !!!dots, .Model_cfg = .Model_cfg),
        mu0 = fit_plugin(df_0, {{ weight_col }}, {{ y_col }}, !!!dots, .Model_cfg = .Model_cfg)
    )
}


#' Fits a treatment effect model using the appropriate settings
#'
#' This function prepares data, fits the appropriate model and returns the
#' resulting estimates in a standardized format.
#' @param full_data The full dataset of interest for the modelling problem.
#' @param weight_col The unquoted weighting variable name to use in model fitting.
#' @param fx_col The unquoted column name of the pseudo-outcome.
#' @param ... The unquoted names of covariates to use in the model.
#' @param .Model_cfg A `Model_cfg` object configuring the appropriate model type to use.
#' @return A list with one element, `fx`. This element contains a `Predictor` object of
#' the appropriate subclass corresponding to the `Model_cfg` fit to the data.
#' @keywords internal
fit_effect <- function(full_data, weight_col, fx_col, ..., .Model_cfg) {
    dots <- rlang::enexprs(...)

    list(
        fx = fit_plugin(full_data, {{ weight_col }}, {{ fx_col }}, !!!dots, .Model_cfg = .Model_cfg)
    )
}
