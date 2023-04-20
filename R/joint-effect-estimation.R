#' Predictor class for the cross-fit predictor of "partial" CATEs
#'
#' The class makes it easier to manage the K predictors for retrieving K-fold
#' cross-validated estimates, as well as to measure how treatment effects change
#' when only a single covariate is changed from its "natural" levels (in the sense
#' "natural" used by the direct / indirect effects literature).
#' @keywords internal
FX.Predictor <- R6::R6Class("FX.Predictor",
    public = list(
        #' @field models A list of the K model fits
        models = list(),
        #' @field num_splits The number of folds used in cross-fitting.
        num_splits = integer(),
        #' @field num_mc_samples The number of samples to retrieve across the covariate space.
        #' If num_mc_samples is larger than the sample size, then the entire dataset will be used.
        num_mc_samples = integer(),
        #' @field covariates The unquoted names of the covariates used in the second-stage model.
        covariates = environment(),
        #' @field model_class The model class (in the sense of `Model_cfg`). For instance,
        #' a SuperLearner model will have model class "SL".
        model_class = character(),
        #' @description
        #' `FX.predictor` is a class which simplifies the management of a set of cross-fit
        #' prediction models of treatment effects and provides the ability to get the "partial"
        #' effects of particular covariates.
        #' @param models A list of the K model fits.
        #' @param num_splits Integer number of cross-fitting folds.
        #' @param num_mc_samples Integer number of Monte-Carlo samples across the covariate
        #' space. If this is larger than the sample size, then the whole dataset will be used.
        #' @param covariates The unquoted names of the covariates.
        #' @param model_class The model class (in the sense of `Model_cfg`).
        initialize = function(models, num_splits, num_mc_samples, covariates, model_class) {
            self$models <- models
            self$num_splits <- num_splits
            self$num_mc_samples <- num_mc_samples
            self$covariates <- covariates
            self$model_class <- model_class
        },
        #' @description
        #' Predicts the PCATE surface over a particular covariate, returning a tibble with
        #' the predicted HTE for every Monte-Carlo sample.
        #' @param data The full dataset
        #' @param covariate The unquoted covariate name for which to calculate predicted
        #' treatment effects.
        #' @return A tibble with columns:
        #' * `covariate_value` - The value of the covariate of interest
        #' * `.hte` - An estimated HTE
        #' * `.id` - The identifier for the original row (which had
        #' `covariate` modified to `covariate_value`).
        predict = function(data, covariate) {
            sample_size <- pmin(self$num_mc_samples[[rlang::as_string(covariate)]], nrow(data))
            unq_values <- unique(data[[rlang::as_string(covariate)]])

            data_modified <- data
            data_list <- list()
            for (idx in seq_along(unq_values)) {
                unq_value <- unq_values[idx]
                data_modified[[rlang::as_string(covariate)]] <- unq_value
                data_list <- c(data_list, list(dplyr::sample_n(data_modified, sample_size)))
            }
            data_aggregated <- dplyr::bind_rows(!!!data_list)

            result <- rep(NA_real_, nrow(data_aggregated))
            for (split_id in seq(self$num_splits)) {
                folds <- split_data(data_aggregated, split_id)
                pred_data <- Model_data$new(folds$holdout, NULL, !!!self$covariates)
                result[folds$in_holdout] <- self$models[[split_id]]$predict(pred_data)$estimate
            }
            o <- dplyr::tibble(
                covariate_value = rep(unq_values, rep(sample_size, length(unq_values))),
                .hte = result,
                .id = data_aggregated[[attr(data, "identifier")]]
            )
            attr(o, "identifier") <- ".id"
            o
        }
    )
)

#' Fit a predictor for treatment effects
#'
#' This function predicts treatment effects in a second stage model.
#' @param full_data The full original data with all auxilliary columns.
#' @param weights Weights to be used in the analysis.
#' @param psi_col The unquoted column name of the calculated pseudo-outcome.
#' @param ... Covariate data, passed in as the unquoted names of columns in `full_data`
#' @param .pcate.cfg A `PCATE_cfg` object describing what PCATEs to calculate (and how)
#' @param .Model_cfg A `Model_cfg` object describing how the effect model should be estimated.
#' @return A list with two items:
#' * `model` - The `FX.Predictor` model object used internally for PCATE estimation.
#' * `data` - The data augmented with column `.pseudo_outcome_hat` for the cross-fit predictions
#' of the HTE for each unit.
#' @seealso [Model_cfg], [PCATE_cfg]
#' @keywords internal
fit_fx_predictor <- function(full_data, weights, psi_col, ...,
    .pcate.cfg, .Model_cfg
) {
    dots <- rlang::enexprs(...)

    num_splits <- max(full_data$.split_id)
    fx_models <- list()
    SL_coefs <- list(
        fx = list()
    )
    fx_hat <- rep(NA_real_, nrow(full_data))

    pb <- progress::progress_bar$new(
        total = num_splits,
        show_after = 0,
        format = "fitting effect models [:bar] splits: :current / :total",
        force = TRUE
    )
    pb$tick(0)
    for (split_id in seq(num_splits)) {
        folds <- split_data(full_data, split_id)
        fx_model <- fit_effect(
            folds$train, {{ weights }}, {{ psi_col }}, !!!dots,
            .Model_cfg = .Model_cfg
        )
        fx_models[[split_id]] <- fx_model$fx
        pred_data <- Model_data$new(folds$holdout, NULL, !!!dots)
        fx_hat[folds$in_holdout] <- fx_model$fx$predict(pred_data)$estimate

        if (.Model_cfg$model_class == "SL") {
            SL_coef <- dplyr::tibble(
                split_id = rep(split_id, length(fx_model$fx$model$libraryNames)),
                model_name = fx_model$fx$model$libraryNames,
                cvRisk = fx_model$fx$model$cvRisk,
                coef = fx_model$fx$model$coef
            )
            SL_coefs[["fx"]] <- c(SL_coefs[["fx"]], list(SL_coef))
        }
        pb$tick()
    }

    full_data$.pseudo_outcome_hat <- fx_hat

    predictor <- FX.Predictor$new(
        models = fx_models,
        num_splits = num_splits,
        num_mc_samples = .pcate.cfg$num_mc_samples,
        covariates = dots,
        model_class = .Model_cfg$model_class
    )

    if (.Model_cfg$model_class == "SL") {
        attr(full_data, "SL_coefs")[["fx"]] <- SL_coefs[["fx"]]
    }

    list(
        model = predictor,
        data = full_data
    )
}
