
#' Construct Pseudo-outcomes
#'
#' `construct_pseudo_outcomes` takes a dataset which has been prepared
#' with plugin estimators of nuisance parameters and transforms these into
#' a "pseudo-outcome": an unbiased estimator of the conditional average
#' treatment effect under exogeneity.
#' @param .data dataframe
#' @param outcome Unquoted name of outcome variable.
#' @param treatment Unquoted name of treatment variable.
#' @export
construct_pseudo_outcomes <- function(.data, outcome, treatment) {
    # check that plugins are in the df
    YA <- unlist(dplyr::select(.data, {{ outcome }}))
    A <- unlist(dplyr::select(.data, {{ treatment }}))
    mu0 <- .data[[".mu0_hat"]]
    mu1 <- .data[[".mu1_hat"]]
    pi <- .data[[".pi_hat"]]
    muA <- A * mu1 + (1 - A) * mu0
    .data$.pseudo_outcome <- (A - pi) / (pi * (1 - pi)) * (YA - muA) + mu1 - mu0
    attr(.data, "treatment") <- rlang::as_string(treatment)
    attr(.data, "outcome") <- rlang::as_string(outcome)
    .data
}

#' @importFrom dplyr summarize
#' @importFrom rlang .data
calculate_ate <- function(.data) {
    id_col <- attr(.data, "identifier")
    dplyr::summarize(
        .data,
        estimand = "ATE",
        estimate = mean(.data$.pseudo_outcome),
        std_error = clustered_se_of_mean(.data$.pseudo_outcome, .data[[id_col]]),
        #sd(.data$.pseudo_outcome) / sqrt(dplyr::n()),
        sample_size = length(unique(.data[[id_col]]))
    )
}


calculate_mcate_quantities <- function(.data, .outcome, ..., .MCATE_cfg) {
    dots <- rlang::enexprs(...)
    .outcome <- rlang::enexpr(.outcome)

    result_list <- list()
    pb <- progress::progress_bar$new(
        total = length(dots),
        show_after = 0,
        format = "estimating MCATEs [:bar] covariates: :current / :total"
    )
    pb$tick(0)
    for (covariate in dots) {
        .Model_cfg <- .MCATE_cfg$cfgs[[rlang::as_string(covariate)]]
        data <- Model_data$new(.data, {{ .outcome }}, {{ covariate }})
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
            result$term <- rlang::quo_name(rlang::enquo(covariate))
            result <- dplyr::select(
                result, .data$term, .data$x, .data$estimate
            )
        }
        if (is.factor(result$x)) {
            names(result)[names(result) == "x"] <- "level"
            result$value <- as.integer(result$level)
        } else if (is.double(result$x) || is.integer(result$x)) {
            names(result)[names(result) == "x"] <- "value"
        } else {
            stop("Unknown type of result!")
        }
        result_list <- c(result_list, list(result))
        pb$tick()
    }

    dplyr::bind_rows(!!!result_list)
}

#' @importFrom rlang .data
calculate_pcate_quantities <- function(.data, .outcome, fx_model, ..., .MCATE_cfg) {
    dots <- rlang::enexprs(...)
    result_list <- list()
    pb <- progress::progress_bar$new(
        total = length(dots),
        format = "estimating PCATEs [:bar] covariates: :current / :total"
    )
    pb$tick(0)
    for (covariate in dots) {
        fx_data <- fx_model$predict(.data, covariate)
        .Model_cfg <- .MCATE_cfg$cfgs[[rlang::as_string(covariate)]]
        data <- Model_data$new(fx_data, .data$.hte, .data$covariate_value)
        predictor <- predictor_factory(.Model_cfg)
        model <- predictor$fit(data)
        if (.MCATE_cfg$std_errors) {
            result <- model$predict_se(data)
            result$term <- rlang::quo_name(rlang::enquo(covariate))
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
        if (is.factor(result$x)) {
            names(result)[names(result) == "x"] <- "level"
            result$value <- as.integer(result$level)
        } else if (is.double(result$x) || is.integer(result$x)) {
            names(result)[names(result) == "x"] <- "value"
        } else {
            stop("Unknown type of result!")
        }
        result_list <- c(result_list, list(result))
        pb$tick()
    }

    dplyr::bind_rows(!!!result_list)
}
