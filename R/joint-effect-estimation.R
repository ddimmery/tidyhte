FX.Predictor <- R6::R6Class("FX.Predictor",
    public = list(
        models = list(),
        num_splits = integer(),
        num_mc_samples = integer(),
        covariates = environment(),
        model_class = character(),
        initialize = function(models, num_splits, num_mc_samples, covariates, model_class) {
            self$models <- models
            self$num_splits <- num_splits
            self$num_mc_samples <- num_mc_samples
            self$covariates <- covariates
            self$model_class <- model_class
        },
        predict = function(data, covariate) {
            sample_size <- pmin(self$num_mc_samples[[rlang::as_string(covariate)]], nrow(data))
            unq_values <- unique(data[[rlang::as_string(covariate)]])

            .data_modified <- data
            data_list <- list()
            for (idx in seq_along(unq_values)) {
                unq_value <- unq_values[idx]
                .data_modified[[rlang::as_string(covariate)]] <- unq_value
                data_list <- c(data_list, list(dplyr::sample_n(.data_modified, sample_size)))
            }
            .data_aggregated <- dplyr::bind_rows(!!!data_list)

            result <- rep(NA_real_, nrow(.data_aggregated))
            for (split_id in seq(self$num_splits)) {
                folds <- split_data(.data_aggregated, split_id)
                pred_data <- Model_data$new(folds$holdout, NULL, !!!self$covariates)
                result[folds$in_holdout] <- self$models[[split_id]]$predict(pred_data)$estimate
            }
            o <- dplyr::tibble(
                covariate_value = rep(unq_values, rep(sample_size, length(unq_values))),
                .hte = result,
                .id = .data_aggregated[[attr(data, "identifier")]]
            )
            attr(o, "identifier") <- ".id"
            o
        }
    )
)


fit_fx_predictor <- function(.data, weights, psi_col, ...,
    .pcate.cfg, .Model_cfg
) {
    dots <- rlang::enexprs(...)

    num_splits <- max(.data$.split_id)
    fx_models <- list()
    SL_coefs <- list(
        fx = list()
    )
    fx_hat <- rep(NA_real_, nrow(.data))

    pb <- progress::progress_bar$new(
        total = num_splits,
        show_after = 0,
        format = "fitting effect models [:bar] splits: :current / :total",
        force = TRUE
    )
    pb$tick(0)
    for (split_id in seq(num_splits)) {
        folds <- split_data(.data, split_id)
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

    .data$.pseudo_outcome_hat <- fx_hat

    predictor <- FX.Predictor$new(
        models = fx_models,
        num_splits = num_splits,
        num_mc_samples = .pcate.cfg$num_mc_samples,
        covariates = dots,
        model_class = .Model_cfg$model_class
    )

    if (.Model_cfg$model_class == "SL") {
        attr(.data, "SL_coefs")[["fx"]] <- SL_coefs[["fx"]]
    }

    list(
        model = predictor,
        data = .data
    )
}
