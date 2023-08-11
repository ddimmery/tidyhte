#' @noRd
#' @keywords internal
SL_model_slot <- function(prediction) {
    if (prediction == ".pi_hat") "pi"
    else if (prediction == ".mu1_hat") "mu1"
    else if (prediction == ".mu0_hat") "mu0"
    else if (prediction == ".pseudo_outcome_hat") "fx"
    else stop("Unknown model slot.")
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
    w_col <- attr(data, "weights")
    id_col <- attr(data, "identifier")

    if (tolower(diag_name) == "auc") {
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
    } else if (tolower(diag_name) == "mse") {
        sqerr <- (data[[label]] - data[[prediction]]) ^ 2
        result <- stats::weighted.mean(sqerr, data[[w_col]])
        stderr <- clustered_se_of_mean(sqerr, data[[id_col]], data[[w_col]])
        result <- dplyr::tibble(
            estimand = "MSE",
            term = label,
            estimate = result,
            std_error = stderr
        )
    } else if (tolower(diag_name) == "sl_coefs") {
        if (
            ("SL_coefs" %in% names(attributes(data))) &&
            (SL_model_slot(prediction) %in% names(attr(data, "SL_coefs"))) &&
            length(attr(data, "SL_coefs")[[SL_model_slot(prediction)]]) > 0
        ) {
        result_list <- attr(data, "SL_coefs")[[SL_model_slot(prediction)]]
        result <- dplyr::bind_rows(!!!result_list) %>%
            dplyr::group_by(.data$model_name) %>%
            dplyr::summarize(
                estimate = mean(.data$coef),
                std_error = stats::sd(.data$coef) / sqrt(dplyr::n()),
                estimand = "SL coefficient"
            ) %>%
            dplyr::rename(term = "model_name")
        } else {
            result <- NULL
            message("Cannot calculate SL_coefs because the model is not SuperLearner.")
        }
    } else if (tolower(diag_name) == "sl_risk") {
        if (
            ("SL_coefs" %in% names(attributes(data))) &&
            (SL_model_slot(prediction) %in% names(attr(data, "SL_coefs"))) &&
            length(attr(data, "SL_coefs")[[SL_model_slot(prediction)]]) > 0
        ) {
        result_list <- attr(data, "SL_coefs")[[SL_model_slot(prediction)]]
        result <- dplyr::bind_rows(!!!result_list) %>%
            dplyr::group_by(.data$model_name) %>%
            dplyr::summarize(
                estimate = mean(.data$cvRisk),
                std_error = stats::sd(.data$cvRisk) / sqrt(dplyr::n()),
                estimand = "SL risk"
            ) %>%
            dplyr::rename(term = "model_name")
        } else {
            result <- NULL
            message("Cannot calculate SL_risk because the model is not SuperLearner.")
        }
    } else if (tolower(diag_name) == "rroc") {
        if ("num_bins" %in% names(params)) {
            nbins <- params$num_bins
        } else {
            nbins <- nrow(data)
        }
        result <- calculate_rroc(data[[label]], data[[prediction]], nbins = nbins)
        result$term <- label
    }
    result
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
