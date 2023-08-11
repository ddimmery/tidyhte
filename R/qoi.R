
#' Construct Pseudo-outcomes
#'
#' `construct_pseudo_outcomes` takes a dataset which has been prepared
#' with plugin estimators of nuisance parameters and transforms these into
#' a "pseudo-outcome": an unbiased estimator of the conditional average
#' treatment effect under exogeneity.
#'
#' Taking averages of these pseudo-outcomes (or fitting a model to them)
#' will approximate averages (or models) of the underlying treatment effect.
#' @param data dataframe (already prepared with `attach_config`, `make_splits`,
#' and `produce_plugin_estimates`)
#' @param outcome Unquoted name of outcome variable.
#' @param treatment Unquoted name of treatment variable.
#' @param type String representing how to construct the pseudo-outcome. Valid
#' values are "dr" (the default), "ipw" and "plugin". See "Details" for more
#' discussion of these options.
#' @seealso [attach_config()], [make_splits()], [produce_plugin_estimates()], [estimate_QoI()]
#' @export
construct_pseudo_outcomes <- function(data, outcome, treatment, type = "dr") {
    outcome <- rlang::enexpr(outcome)
    treatment <- rlang::enexpr(treatment)

    YA <- unlist(dplyr::select(data, {{ outcome }}))
    A <- unlist(dplyr::select(data, {{ treatment }}))
    mu0 <- data[[".mu0_hat"]]
    mu1 <- data[[".mu1_hat"]]
    pi <- data[[".pi_hat"]]
    data$.pseudo_outcome <- pseudo_outcome_factory(type)(A, YA, pi, mu1, mu0)
    attr(data, "treatment") <- rlang::as_name(treatment)
    attr(data, "outcome") <- rlang::as_name(outcome)
    data
}


#' @noRd
#' @keywords internal
pseudo_outcome_factory <- function(type) {
    type <- tolower(type)
    if (type == "dr") {
        dr_pseudo_outcome
    } else if (type == "ipw") {
        ipw_pseudo_outcome
    } else if (type == "plugin") {
        plugin_pseudo_outcome
    } else {
        stop("Unknown type of pseudo-outcome.")
    }
}

#' @noRd
#' @keywords internal
dr_pseudo_outcome <- function(A, YA, pi, mu1, mu0) {
    muA <- muA <- A * mu1 + (1 - A) * mu0
    (A - pi) / (pi * (1 - pi)) * (YA - muA) + mu1 - mu0
}

#' @noRd
#' @keywords internal
ipw_pseudo_outcome <- function(A, YA, pi, mu1, mu0) {
    (A - pi) / (pi * (1 - pi)) * YA
}

#' @noRd
#' @keywords internal
plugin_pseudo_outcome <- function(A, YA, pi, mu1, mu0) {
    mu1 - mu0
}


#' Calculates a SATE and a PATE using AIPW
#'
#' This function takes fully prepared data (with all auxilliary columns from the
#' necessary models) and estimates average treatment effects using AIPW.
#' @param data The dataset of interest after it has been prepared fully.
#' @seealso [basic_config()], [attach_config()], [make_splits()], [produce_plugin_estimates()],
#' [construct_pseudo_outcomes()], [estimate_QoI()]
#' @references
#' * Kennedy, E. H. (2020). Towards optimal doubly robust estimation of heterogeneous
#' causal effects. *arXiv preprint arXiv:2004.14497*.
#' * Tsiatis, A. A., Davidian, M., Zhang, M., & Lu, X. (2008). Covariate adjustment
#' for two‐sample treatment comparisons in randomized clinical trials: a principled
#' yet flexible approach. *Statistics in medicine*, 27(23), 4658-4677.
#' @importFrom dplyr summarize
#' @importFrom stats weighted.mean
#' @keywords internal
calculate_ate <- function(data) {
    id_col <- attr(data, "identifier")
    w_col <- attr(data, "weights")
    o <- dplyr::summarize(
        data,
        estimand = "SATE",
        estimate = mean(data$.pseudo_outcome),
        std_error = clustered_se_of_mean(data$.pseudo_outcome, data[[id_col]]),
        sample_size = length(unique(data[[id_col]]))
    )
    if (!zero_range(data[[w_col]])) {
        o <- dplyr::bind_rows(
            o,
            dplyr::summarize(
                data,
                estimand = "PATE",
                estimate = stats::weighted.mean(data$.pseudo_outcome, data[[w_col]]),
                std_error = clustered_se_of_mean(
                    data$.pseudo_outcome, data[[id_col]], data[[w_col]]
                ),
                sample_size = sum(data[[w_col]])
            )
        )
    }
    o
}

#' @noRd
#' @keywords internal
calculate_mcate_quantities <- function(full_data, .weights, .outcome, ..., .MCATE_cfg) {
    dots <- rlang::enexprs(...)
    .outcome <- rlang::enexpr(.outcome)
    .weights <- rlang::enexpr(.weights)

    result_list <- list()
    pb <- progress::progress_bar$new(
        total = length(dots),
        show_after = 0,
        format = "estimating MCATEs [:bar] covariates: :current / :total",
        force = TRUE
    )
    pb$tick(0)
    for (covariate in dots) {
        .Model_cfg <- .MCATE_cfg$cfgs[[rlang::as_name(covariate)]]
        data <- Model_data$new(
            full_data, {{ .outcome }}, {{ covariate }}, .weight_col = {{ .weights }}
        )
        predictor <- predictor_factory(.Model_cfg)
        model <- predictor$fit(data)
        if (.MCATE_cfg$std_errors) {
            result <- model$predict_se(data)
            result$term <- rlang::quo_name(rlang::enquo(covariate))
            result <- dplyr::select(
                result, "term", "x", "estimate", "std_error"
            )
        } else {
            result <- model$predict(data)
            result$term <- rlang::as_name(rlang::enquo(covariate))
            result <- dplyr::select(
                result, "term", "x", "estimate"
            )
        }
        if (is.double(result$x) || is.integer(result$x) || is.character(result$x)) {
            names(result)[names(result) == "x"] <- "value"
        } else {
            names(result)[names(result) == "x"] <- "level"
            result$level <- as.character(result$level)
        }
        result_list <- c(result_list, list(result))
        pb$tick()
    }
    dplyr::bind_rows(!!!result_list)
}

#' Calculate "partial" CATE estimates
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' @keywords internal
calculate_pcate_quantities <- function(full_data, .weights, .outcome, fx_model, ..., .MCATE_cfg) {
    lifecycle::signal_stage("experimental", "calculate_pcate_quantities()")
    dots <- rlang::enexprs(...)
    result_list <- list()
    pb <- progress::progress_bar$new(
        total = length(dots),
        show_after = 0,
        format = "estimating PCATEs [:bar] covariates: :current / :total",
        force = TRUE
    )
    pb$tick(0)
    for (covariate in dots) {
        fx_data <- fx_model$predict(full_data, covariate)
        .Model_cfg <- .MCATE_cfg$cfgs[[rlang::as_name(covariate)]]
        data <- Model_data$new(fx_data, ".hte", "covariate_value")
        predictor <- predictor_factory(.Model_cfg)
        model <- predictor$fit(data)
        if (.MCATE_cfg$std_errors) {
            result <- model$predict_se(data)
            result$term <- rlang::as_name(rlang::enquo(covariate))
            result <- dplyr::select(
                result, "term", "x", "estimate", "std_error", "sample_size"
            )
            mse <- mean((full_data$.pseudo_outcome_hat - full_data$.pseudo_outcome) ^ 2)
            result$std_error <- sqrt(result$std_error ^ 2 + mse / result$sample_size)
        } else {
            result <- model$predict(data)
            result$term <- rlang::quo_name(rlang::enquo(covariate))
            result <- dplyr::select(
                result, "term", "x", "estimate", "sample_size"
            )
        }
        if (is.double(result$x) || is.integer(result$x)) {
            names(result)[names(result) == "x"] <- "value"
        } else {
            names(result)[names(result) == "x"] <- "level"
            result$value <- as.integer(result$level)
        }
        result_list <- c(result_list, list(result))
        pb$tick()
    }

    dplyr::bind_rows(!!!result_list)
}
