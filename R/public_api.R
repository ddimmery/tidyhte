#' Define splits for cross-fitting
#'
#' This takes a dataset, a column with a unique identifier and an
#' arbitrary number of covariates on which to stratify the splits.
#' It returns the original dataset with an additional column `.split_id`
#' corresponding to an identifier for the split.
#' @param .data dataframe
#' @param id_col unquoted name of unique identifier column
#' @param ... variables on which to stratify
#' @param .num_splits number of splits to create
#' @return original dataframe with additional `.split_id` column
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @export
make_splits <- function(.data, id_col, ..., .num_splits) {
    .data %>%
    dplyr::group_by({{id_col}}, ...) %>%
    dplyr::tally() %>%
    dplyr::group_by(...) %>%
    dplyr::mutate(
        .split_id = sample(c(
            rep(
                1:.num_splits,
                rep(as.integer(floor(dplyr::n() / .num_splits), .num_splits))
            ),
            sample(
                .num_splits,
                (
                    dplyr::n() -
                    .num_splits * as.integer(floor(dplyr::n() / .num_splits))
                )
            )
        ))
    ) %>%
    dplyr::select(-.data$n) -> tmp
    join_cols <- names(dplyr::select(tmp, -.data$.split_id))
    dplyr::left_join(.data, tmp, by = join_cols)
}

#' Estimate models of nuisance functions
#'
#' This takes a dataset with an identified outcome and treatment column along
#' with any number of covariates and appends three columns to the dataset corresponding
#' to an estimate of the conditional expectation of treatment (`.pi_hat`), along with the
#' conditional expectation of the control and treatment potential outcome surfaces
#' (`.mu0_hat` and `.mu1_hat` respectively).
#' @param .data dataframe
#' @param y_col Unquoted name of the outcome variable.
#' @param a_col Unquoted name of the treatment variable.
#' @param ... Unquoted names of covariates to include in the models of the nuisance functions.
#' @param .HTE_cfg `HTE_cfg` object representing the full configuration of the HTE analysis.
#' @export
produce_plugin_estimates <- function(.data, y_col, a_col, ..., .HTE_cfg=NULL) {
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

        a_model <- fit_plugin_A(
            folds$train, {{ a_col }}, !!!dots,
            .Model_cfg = .HTE_cfg$treatment
        )

        if (.HTE_cfg$treatment$model_class == "SL") {
            SL_coef <- dplyr::tibble(
                split_id = rep(split_id, length(a_model$pi$model$libraryNames)),
                model_name = a_model$pi$model$libraryNames,
                cvRisk = a_model$pi$model$cvRisk,
                coef = a_model$pi$model$coef
            )
            SL_coefs[["pi"]] <- c(SL_coefs[["pi"]], list(SL_coef))
        }

        y_model <- fit_plugin_Y(
            folds$train, {{ y_col }}, {{ a_col }}, !!!dots,
            .Model_cfg = .HTE_cfg$outcome
        )

        if (.HTE_cfg$outcome$model_class == "SL") {
            SL_coef <- dplyr::tibble(
                split_id = rep(split_id, length(y_model$mu0$model$libraryNames)),
                model_name = y_model$mu0$model$libraryNames,
                cvRisk = y_model$mu0$model$cvRisk,
                coef = y_model$mu0$model$coef
            )
            SL_coefs[["mu0"]] <- c(SL_coefs[["mu0"]], list(SL_coef))

            SL_coef <- dplyr::tibble(
                split_id = rep(split_id, length(y_model$mu1$model$libraryNames)),
                model_name = y_model$mu1$model$libraryNames,
                cvRisk = y_model$mu1$model$cvRisk,
                coef = y_model$mu1$model$coef
            )
            SL_coefs[["mu1"]] <- c(SL_coefs[["mu1"]], list(SL_coef))
        }


        if (.HTE_cfg$treatment$model_class == "known") {
            cov <- rlang::sym(.HTE_cfg$treatment$covariate_name)
            dots <- c(dots, cov)
        }
        pred_data <- Model_data$new(folds$holdout, NULL, !!!dots)
        pi_hat[folds$in_holdout] <- a_model$pi$predict(pred_data)

        if (.HTE_cfg$treatment$model_class == "known") {
            dots <- rlang::enexprs(...)
            pred_data <- Model_data$new(folds$holdout, NULL, !!!dots)
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

#' @export
#' @importFrom rlang .env
estimate_QoI <- function(
    .data, .outcome, .treatment, ..., .HTE_cfg=NULL
) {
    dots <- rlang::enexprs(...)

    .QoI_cfg <- .HTE_cfg$qoi
    result_list <- list()

    if (!is.null(.QoI_cfg$mcate)) {
        result <- calculate_mcate_quantities(
            .data,
            .data$.pseudo_outcome,
            !!!dots,
            .MCATE_cfg = .QoI_cfg$mcate
        )
        result_list <- c(result_list, list(dplyr::mutate(result, estimand = "MCATE")))
    }

    if (!is.null(.QoI_cfg$vimp)) {
        result <- calculate_vimp(.data, .data$.pseudo_outcome, !!!dots, .VIMP_cfg = .QoI_cfg$vimp)
        result_list <- c(result_list, list(result))
    }

    if (!is.null(.QoI_cfg$pcate)) {
        covs <- rlang::syms(.QoI_cfg$pcate$model_covariates)
        fx_mod <- fit_fx_predictor(.data, .data$.pseudo_outcome, !!!covs, .pcate.cfg = .QoI_cfg$pcate)
        .data <- fx_mod$data
        result <- calculate_pcate_quantities(
            .data,
            .data$.pseudo_outcome,
            fx_mod$model,
            !!!dots,
            .MCATE_cfg = .QoI_cfg$mcate
        )
        result_list <- c(result_list, list(dplyr::mutate(result, estimand = "PCATE")))
    }

    if (!is.null(.QoI_cfg$diag)) {
        result <- calculate_diagnostics(.data, .diag.cfg = .QoI_cfg$diag)
        result_list <- c(result_list, list(result))
    }

    col_order <- c("estimand", "term", "value", "level", "estimate", "std_error")

    dplyr::bind_rows(!!!result_list) %>%
        dplyr::select(intersect(col_order, names(.env$.)))
}
