#'
#' @export
produce.plugin.estimates <- function(.data, y_col, a_col, ...,
    .HTE.cfg=NULL
) {
    if (is.null(.HTE.cfg)) .HTE.cfg <- HTE.cfg$new()
    dots <- rlang::enexprs(...)

    num_splits <- max(.data$.split_id)
    pi_hat <- rep(NA_real_, nrow(.data))
    mu0_hat <- rep(NA_real_, nrow(.data))
    mu1_hat <- rep(NA_real_, nrow(.data))
    SL_coefs <- list(
        pi = list(),
        mu0 = list(),
        mu1 = list()
    )

    for (split_id in 1:(num_splits - 1)) {
        folds <- split_data(.data, split_id)

        a_model <- fit.plugin.A(
            folds$train, {{ a_col }}, !!!dots,
            .Model.cfg = .HTE.cfg$treatment
        )

        if (.HTE.cfg$treatment$model_class == "SL") {
            SL_coef <- tibble(
                split_id = rep(split_id, length(a_model$pi$model$libraryNames)),
                model_name = a_model$pi$model$libraryNames,
                cvRisk = a_model$pi$model$cvRisk,
                coef = a_model$pi$model$coef
            )
            SL_coefs[["pi"]] <- c(SL_coefs[["pi"]], list(SL_coef))
        }

        y_model <- fit.plugin.Y(
            folds$train, {{ y_col }}, {{ a_col }}, !!!dots,
            .Model.cfg = .HTE.cfg$outcome
        )

        if (.HTE.cfg$outcome$model_class == "SL") {
            SL_coef <- tibble(
                split_id = rep(split_id, length(y_model$mu0$model$libraryNames)),
                model_name = y_model$mu0$model$libraryNames,
                cvRisk = y_model$mu0$model$cvRisk,
                coef = y_model$mu0$model$coef
            )
            SL_coefs[["mu0"]] <- c(SL_coefs[["mu0"]], list(SL_coef))

            SL_coef <- tibble(
                split_id = rep(split_id, length(y_model$mu1$model$libraryNames)),
                model_name = y_model$mu1$model$libraryNames,
                cvRisk = y_model$mu1$model$cvRisk,
                coef = y_model$mu1$model$coef
            )
            SL_coefs[["mu1"]] <- c(SL_coefs[["mu1"]], list(SL_coef))
        }


        if (.HTE.cfg$treatment$model_class == "known") {
            cov <- rlang::sym(.HTE.cfg$treatment$covariate_name)
            dots <- c(dots, cov)
        }
        pred_data <- Model.data$new(folds$holdout, NULL, !!!dots)
        pi_hat[folds$in_holdout] <- a_model$pi$predict(pred_data)

        if (.HTE.cfg$treatment$model_class == "known") {
            dots <- rlang::enexprs(...)
            pred_data <- Model.data$new(folds$holdout, NULL, !!!dots)
        }

        mu1_hat[folds$in_holdout] <- y_model$mu1$predict(pred_data)
        mu0_hat[folds$in_holdout] <- y_model$mu0$predict(pred_data)
    }
    .data$.pi_hat <- pi_hat
    .data$.mu1_hat <- mu1_hat
    .data$.mu0_hat <- mu0_hat
    attr(.data, "SL_coefs") <- SL_coefs
    .data
}

FX.Predictor <- R6::R6Class("FX.Predictor",
    public = list(
        models = list(),
        num_splits = integer(),
        num_mc_samples = integer(),
        covariates = environment(),
        initialize = function(models, num_splits, num_mc_samples, covariates) {
            self$models <- models
            self$num_splits <- num_splits
            self$num_mc_samples <- num_mc_samples
            self$covariates <- covariates
        },
        predict = function(data, covariate) {
            sample_size <- pmin(self$num_mc_samples, nrow(data))
            unq_values <- unique(data[[rlang::as_string(covariate)]])
            result <- rep(NA_real_, length(unq_values) * sample_size)
            .data_modified <- data
            for (idx in seq_along(unq_values)) {
                unq_value <- unq_values[idx]
                .data_modified[[rlang::as_string(covariate)]] <- unq_value
                for (split_id in 1:(num_splits - 1)) {
                    folds <- split_data(sample_n(.data_modified, sample_size), split_id)
                    pred_data <- Model.data$new(folds$holdout, NULL, !!!self$covariates)
                    result_idx <- (idx - 1) * samplesize + folds$in_holdout
                    result[result_idx] <- self$models[[split_id]]$fx$predict(pred_data)
                }
            }

            tibble(
                covariate_value = rep(unq_values, rep(sample_size, length(unq_values))),
                .hte = result
            )
        }
    )
)


#'
#' @export
fit.fx.predictor <- function(.data, psi_col, a_col, ...,
    .pcate.cfg=NULL
) {
    dots <- rlang::enexprs(...)

    num_splits <- max(.data$.split_id)
    fx_hat <- rep(NA_real_, nrow(.data))
    fx_models <- list()
    SL_coefs <- list(
        fx = list()
    )
    for (split_id in 1:(num_splits - 1)) {
        folds <- split_data(.data, split_id)
        fx_model <- fit.effect(
            folds$train, {{ psi_col }}, !!!dots,
            .Model.cfg = .pcate.cfg$effect_cfg
        )
        fx_models[[split_id]] <- fx_model$fx

        if (.pcate.cfg$effect_cfg$model_class == "SL") {
            SL_coef <- tibble(
                split_id = rep(split_id, length(fx_model$fx$model$libraryNames)),
                model_name = fx_model$fx$model$libraryNames,
                cvRisk = fx_model$fx$model$cvRisk,
                coef = fx_model$fx$model$coef
            )
            SL_coefs[["fx"]] <- c(SL_coefs[["fx"]], list(SL_coef))
        }
    }

    FX.Predictor$new(
        models = fx_models,
        num_splits = num_splits,
        num_mc_samples = .pcate.cfg$num_mc_samples,
        covariates = dots
    )
}


#' @export
HTE.cfg <- R6::R6Class("HTE_cfg",
    list(
        outcome = list(),
        treatment = list(),
        qoi = list(),
        initialize = function(
            outcome=NULL, treatment=NULL, qoi=NULL
        ) {
            if (is.null(outcome)) outcome <- SL.cfg$new()
            if (is.null(treatment)) treatment <- SL.cfg$new()
            if (is.null(qoi)) qoi <- QoI.cfg$new()
            self$outcome <- outcome
            self$treatment <- treatment
            self$qoi <- qoi
        }
    )
)