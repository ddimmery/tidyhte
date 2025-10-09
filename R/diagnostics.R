#' @noRd
#' @keywords internal
SL_model_slot <- function(prediction) {
    if (prediction == ".pi_hat") "pi"
    else if (prediction == ".mu1_hat") "mu1"
    else if (prediction == ".mu0_hat") "mu0"
    else if (prediction == ".pseudo_outcome_hat") "fx"
    else abort_model("Unknown model slot.")
}

#' Calculate AUC diagnostic
#' @noRd
#' @keywords internal
calculate_auc_diagnostic <- function(data, label, prediction) {
    w_col <- attr(data, "weights")
    soft_require("WeightedROC")
    labels <- data[[label]]
    if (checkmate::test_integerish(labels, lower = 0, upper = 1)) {
        predictions <- data[[prediction]]
        n1 <- sum(data[[label]] * data[[w_col]])
        n <- sum(data[[w_col]])
        wroc <- WeightedROC::WeightedROC(predictions, labels, data[[w_col]])
        auc <- WeightedROC::WeightedAUC(wroc)
        result <- dplyr::tibble(
            estimand = "AUC",
            term = label,
            estimate = auc,
            # Hanley and McNeil (1982) bound on the variance of AUC
            std_error = 1 / 2 / sqrt(pmin(n1, n - n1))
        )
    } else {
        result <- NULL
        message("Cannot calculate AUC because labels are not binary.")
    }
    result
}

#' Calculate MSE diagnostic
#' @noRd
#' @keywords internal
calculate_mse_diagnostic <- function(data, label, prediction) {
    w_col <- attr(data, "weights")
    id_col <- attr(data, "identifier")
    sqerr <- (data[[label]] - data[[prediction]]) ^ 2
    result <- stats::weighted.mean(sqerr, data[[w_col]])
    stderr <- clustered_se_of_mean(sqerr, data[[id_col]], data[[w_col]])
    dplyr::tibble(
        estimand = "MSE",
        term = label,
        estimate = result,
        std_error = stderr
    )
}

#' Extract SuperLearner model information
#' @noRd
#' @keywords internal
extract_sl_data <- function(data, prediction, field_name) {
    if (
        ("SL_coefs" %in% names(attributes(data))) &&
        (SL_model_slot(prediction) %in% names(attr(data, "SL_coefs"))) &&
        length(attr(data, "SL_coefs")[[SL_model_slot(prediction)]]) > 0
    ) {
        result_list <- attr(data, "SL_coefs")[[SL_model_slot(prediction)]]
        result <- dplyr::bind_rows(!!!result_list) %>%
            dplyr::group_by(.data$model_name) %>%
            dplyr::summarize(
                estimate = mean(.data[[field_name]]),
                std_error = stats::sd(.data[[field_name]]) / sqrt(dplyr::n())
            ) %>%
            dplyr::rename(term = "model_name")
        result
    } else {
        NULL
    }
}

#' Calculate SuperLearner coefficients diagnostic
#' @noRd
#' @keywords internal
calculate_sl_coefs_diagnostic <- function(data, label, prediction) {
    result <- extract_sl_data(data, prediction, "coef")
    if (!is.null(result)) {
        result$estimand <- "SL coefficient"
    } else {
        message("Cannot calculate SL_coefs because the model is not SuperLearner.")
    }
    result
}

#' Calculate SuperLearner risk diagnostic
#' @noRd
#' @keywords internal
calculate_sl_risk_diagnostic <- function(data, label, prediction) {
    result <- extract_sl_data(data, prediction, "cvRisk")
    if (!is.null(result)) {
        result$estimand <- "SL risk"
    } else {
        message("Cannot calculate SL_risk because the model is not SuperLearner.")
    }
    result
}

#' Calculate RROC diagnostic
#' @noRd
#' @keywords internal
calculate_rroc_diagnostic <- function(data, label, prediction, params) {
    if ("num_bins" %in% names(params)) {
        nbins <- params$num_bins
    } else {
        nbins <- nrow(data)
    }
    result <- calculate_rroc(data[[label]], data[[prediction]], nbins = nbins)
    result$term <- label
    result
}

#' Function to calculate diagnostics based on model outputs
#'
#' This function defines the calculations of common model diagnostics
#' which are available.
#' @param data The full data frame with all auxilliary columns.
#' @param label The (string) column name for the labels to evaluate against.
#' @param prediction The (string) column name of predictions from the model to diagnose.
#' @param diag_name The (string) name of the diagnostic to calculate. Currently
#' available are "AUC", "MSE", "SL_coefs", "SL_risk", "RROC"
#' @param params Any other necessary options to pass to the given diagnostic.
#' @examples
#' df <- dplyr::tibble(y = rbinom(100, 1, 0.5), p = rep(0.5, 100), w = rexp(100), u = 1:100)
#' attr(df, "weights") <- "w"
#' attr(df, "identifier") <- "u"
#' estimate_diagnostic(df, "y", "p", "AUC")
#' @keywords internal
#' @export
#' @importFrom stats sd weighted.mean
estimate_diagnostic <- function(data, label, prediction, diag_name, params) {
    diag_name_lower <- tolower(diag_name)

    switch(diag_name_lower,
        "auc" = calculate_auc_diagnostic(data, label, prediction),
        "mse" = calculate_mse_diagnostic(data, label, prediction),
        "sl_coefs" = calculate_sl_coefs_diagnostic(data, label, prediction),
        "sl_risk" = calculate_sl_risk_diagnostic(data, label, prediction),
        "rroc" = calculate_rroc_diagnostic(data, label, prediction, params),
        stop("Unknown diagnostic: ", diag_name)
    )
}

#' Calculate diagnostics
#'
#' This function calculates the diagnostics requested by the `Diagnostics_cfg` object.
#' @param data Data frame with all additional columns (such as model predictions) included.
#' @param treatment Unquoted treatment variable name
#' @param outcome Unquoted outcome variable name
#' @param .diag.cfg `Diagnostics_cfg` object
#' @return Returns a tibble with columns:
#' * `estimand` - Character indicating the diagnostic that was calculated
#' * `level` - Indicates the scope of this diagnostic (e.g. does it apply
#' only to the model of the outcome under treatment).
#' * `term` - Indicates a more granular descriptor of what the value is for,
#' such as the specific model within the SuperLearner ensemble.
#' * `estimate` - Point estimate of the diagnostic.
#' * `std_error` - Standard error of the diagnostic.
#' @seealso [Diagnostics_cfg]
#' @keywords internal
calculate_diagnostics <- function(data, treatment, outcome, .diag.cfg) {
    ps_cfg <- .diag.cfg$ps
    y_cfg <- .diag.cfg$outcome
    fx_cfg <- .diag.cfg$effect
    params <- .diag.cfg$params
    treatment_name <- attr(data, "treatment")
    outcome_name <- attr(data, "outcome")

    result_list <- list()
    for (diag in ps_cfg) {
        result <- estimate_diagnostic(data, treatment_name, ".pi_hat", diag, params)
        result$level <- "Propensity Score"
        result_list <- c(result_list, list(result))
    }

    for (diag in y_cfg) {
        result1 <- estimate_diagnostic(
            dplyr::filter(data, data[[treatment_name]] == 1),
            outcome_name,
            ".mu1_hat",
            diag,
            params
        )
        result1$level <- "Treatment Response"
        result0 <- estimate_diagnostic(
            dplyr::filter(data, data[[treatment_name]] == 0),
            outcome_name,
            ".mu0_hat",
            diag,
            params
        )
        result0$level <- "Control Response"
        result_list <- c(result_list, list(result0), list(result1))
    }

    for (diag in fx_cfg) {
        if (!(".pseudo_outcome_hat" %in% names(data))) {
            message(paste("Skipping diagnostic on .pseudo_outcome due to lack of model."))
            break
        }
        result <- estimate_diagnostic(data, ".pseudo_outcome", ".pseudo_outcome_hat", diag, params)
        result$level <- "Effect Surface"
        result_list <- c(result_list, list(result))
    }

    dplyr::bind_rows(!!!result_list)
}
