
#' Construct Pseudo-outcomes
#'
#' `construct_pseudo_outcomes` takes a dataset which has been prepared
#' with plugin estimators of nuisance parameters and transforms these into
#' a "pseudo-outcome": an unbiased estimator of the conditional average
#' treatment effect under exogeneity.
#' @param .data dataframe (already prepared with `attach_config`, `make_splits`,
#' and `produce_plugin_estimates`)
#' @param outcome Unquoted name of outcome variable.
#' @param treatment Unquoted name of treatment variable.
#' @export
construct_pseudo_outcomes <- function(.data, outcome, treatment, type = "dr") {
    YA <- unlist(dplyr::select(.data, {{ outcome }}))
    A <- unlist(dplyr::select(.data, {{ treatment }}))
    mu0 <- .data[[".mu0_hat"]]
    mu1 <- .data[[".mu1_hat"]]
    pi <- .data[[".pi_hat"]]
    .data$.pseudo_outcome <- pseudo_outcome_factory(type)(A, YA, pi, mu1, mu0)
    attr(.data, "treatment") <- rlang::as_string(treatment)
    attr(.data, "outcome") <- rlang::as_string(outcome)
    .data
}

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

dr_pseudo_outcome <- function(A, YA, pi, mu1, mu0) {
    muA <- muA <- A * mu1 + (1 - A) * mu0
    (A - pi) / (pi * (1 - pi)) * (YA - muA) + mu1 - mu0
}

ipw_pseudo_outcome <- function(A, YA, pi, mu1, mu0) {
    (A - pi) / (pi * (1 - pi)) * YA
}

plugin_pseudo_outcome <- function(A, YA, pi, mu1, mu0) {
    mu1 - mu0
}

#' @importFrom dplyr summarize
#' @importFrom stats weighted.mean
#' @importFrom rlang .data
calculate_ate <- function(.data) {
    id_col <- attr(.data, "identifier")
    w_col <- attr(.data, "weights")
    o <- dplyr::summarize(
        .data,
        estimand = "SATE",
        estimate = mean(.data$.pseudo_outcome),
        std_error = clustered_se_of_mean(.data$.pseudo_outcome, .data[[id_col]]),
        sample_size = length(unique(.data[[id_col]]))
    )
    if (!zero_range(.data[[w_col]])) {
        o <- dplyr::bind_rows(
            o,
            dplyr::summarize(
                .data,
                estimand = "PATE",
                estimate = stats::weighted.mean(.data$.pseudo_outcome, .data[[w_col]]),
                std_error = clustered_se_of_mean(.data$.pseudo_outcome, .data[[id_col]], .data[[w_col]]),
                sample_size = sum(.data[[w_col]])
            )
        )
    }
    o
}


calculate_mcate_quantities <- function(.data, .weights, .outcome, ..., .MCATE_cfg) {
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
        data <- Model_data$new(.data, {{ .outcome }}, {{ covariate }}, .weight_col = {{ .weights }})
        predictor <- predictor_factory(.Model_cfg)
        model <- predictor$fit(data)
        if (.MCATE_cfg$std_errors) {
            result <- model$predict_se(data)
            result$term <- rlang::quo_name(rlang::enquo(covariate))
            result <- dplyr::select(
                result, .data$term, .data$x, .data$estimate, .data$std_error
            )
        } else {
            result <- model$predict(data)
            result$term <- rlang::as_name(rlang::enquo(covariate))
            result <- dplyr::select(
                result, .data$term, .data$x, .data$estimate
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

#' @importFrom rlang .data
calculate_pcate_quantities <- function(.data, .weights, .outcome, fx_model, ..., .MCATE_cfg) {
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
        fx_data <- fx_model$predict(.data, covariate)
        .Model_cfg <- .MCATE_cfg$cfgs[[rlang::as_name(covariate)]]
        data <- Model_data$new(fx_data, .data$.hte, .data$covariate_value)
        predictor <- predictor_factory(.Model_cfg)
        model <- predictor$fit(data)
        if (.MCATE_cfg$std_errors) {
            result <- model$predict_se(data)
            result$term <- rlang::as_name(rlang::enquo(covariate))
            result <- dplyr::select(
                result, .data$term, .data$x, .data$estimate, .data$std_error, .data$sample_size
            )
            mse <- mean((.data$.pseudo_outcome_hat - .data$.pseudo_outcome) ^ 2)
            result$std_error <- sqrt(result$std_error ^ 2 + mse / result$sample_size)
        } else {
            result <- model$predict(data)
            result$term <- rlang::quo_name(rlang::enquo(covariate))
            result <- dplyr::select(
                result, .data$term, .data$x, .data$estimate, .data$sample_size
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
