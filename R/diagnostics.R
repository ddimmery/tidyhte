SL_model_slot <- function(prediction) {
    if (prediction == ".pi_hat") "pi"
    else if (prediction == ".mu1_hat") "mu1"
    else if (prediction == ".mu0_hat") "mu0"
    else if (prediction == ".pseudo_outcome_hat") "fx"
    else stop("Unknown model slot.")
}


#' @importFrom stats sd weighted.mean
#' @importFrom rlang .data
estimate_diagnostic <- function(.data, label, prediction, diag_name, params) {
    w_col <- attr(.data, "weights")
    id_col <- attr(.data, "identifier")

    if (tolower(diag_name) == "auc") {
        soft_require("WeightedROC")
        labels <- .data[[label]]
        if (checkmate::test_integerish(labels, lower = 0, upper = 1)) {
            predictions <- .data[[prediction]]
            n1 <- sum(.data[[label]] * .data[[w_col]])
            n <- sum(.data[[w_col]])
            wroc <- WeightedROC::WeightedROC(predictions, labels, .data[[w_col]])
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
        sqerr <- (.data[[label]] - .data[[prediction]]) ^ 2
        result <- stats::weighted.mean(sqerr, .data[[w_col]])
        stderr <- clustered_se_of_mean(sqerr, .data[[id_col]], .data[[w_col]])
        result <- dplyr::tibble(
            estimand = "MSE",
            term = label,
            estimate = result,
            std_error = stderr
        )
    } else if (tolower(diag_name) == "sl_coefs") {
        if (
            ("SL_coefs" %in% names(attributes(.data))) &&
            (SL_model_slot(prediction) %in% names(attr(.data, "SL_coefs"))) &&
            length(attr(.data, "SL_coefs")[[SL_model_slot(prediction)]]) > 0
        ) {
        result_list <- attr(.data, "SL_coefs")[[SL_model_slot(prediction)]]
        result <- dplyr::bind_rows(!!!result_list) %>%
            dplyr::group_by(.data$model_name) %>%
            dplyr::summarize(
                estimate = mean(.data$coef),
                std_error = stats::sd(.data$coef) / sqrt(dplyr::n()),
                estimand = "SL coefficient"
            ) %>%
            dplyr::rename(term = .data$model_name)
        } else {
            result <- NULL
            message("Cannot calculate SL_coefs because the model is not SuperLearner.")
        }
    } else if (tolower(diag_name) == "sl_risk") {
        if (
            ("SL_coefs" %in% names(attributes(.data))) &&
            (SL_model_slot(prediction) %in% names(attr(.data, "SL_coefs"))) &&
            length(attr(.data, "SL_coefs")[[SL_model_slot(prediction)]]) > 0
        ) {
        result_list <- attr(.data, "SL_coefs")[[SL_model_slot(prediction)]]
        result <- dplyr::bind_rows(!!!result_list) %>%
            dplyr::group_by(.data$model_name) %>%
            dplyr::summarize(
                estimate = mean(.data$cvRisk),
                std_error = stats::sd(.data$cvRisk) / sqrt(dplyr::n()),
                estimand = "SL risk"
            ) %>%
            dplyr::rename(term = .data$model_name)
        } else {
            result <- NULL
            message("Cannot calculate SL_risk because the model is not SuperLearner.")
        }
    } else if (tolower(diag_name) == "rroc") {
        if ("num_bins" %in% names(params)) {
            nbins <- params$num_bins
        } else {
            nbins <- nrow(.data)
        }
        result <- calculate_rroc(.data[[label]], .data[[prediction]], nbins = nbins)
        result$term <- label
    }
    result
}


calculate_diagnostics <- function(.data, treatment, outcome, .diag.cfg) {
    ps_cfg <- .diag.cfg$ps
    y_cfg <- .diag.cfg$outcome
    fx_cfg <- .diag.cfg$effect
    params <- .diag.cfg$params
    treatment_name <- attr(.data, "treatment")
    outcome_name <- attr(.data, "outcome")

    result_list <- list()
    for (diag in ps_cfg) {
        result <- estimate_diagnostic(.data, treatment_name, ".pi_hat", diag, params)
        result$level <- "Propensity Score"
        result_list <- c(result_list, list(result))
    }

    for (diag in y_cfg) {
        result1 <- estimate_diagnostic(
            dplyr::filter(.data, .data[[treatment_name]] == 1),
            outcome_name,
            ".mu1_hat",
            diag,
            params
        )
        result1$level <- "Treatment Response"
        result0 <- estimate_diagnostic(
            dplyr::filter(.data, .data[[treatment_name]] == 0),
            outcome_name,
            ".mu0_hat",
            diag,
            params
        )
        result0$level <- "Control Response"
        result_list <- c(result_list, list(result0), list(result1))
    }

    for (diag in fx_cfg) {
        if (!(".pseudo_outcome_hat" %in% names(.data))) {
            message(paste("Skipping diagnostic on .pseudo_outcome due to lack of model."))
            break
        }
        result <- estimate_diagnostic(.data, ".pseudo_outcome", ".pseudo_outcome_hat", diag, params)
        result$level <- "Effect Surface"
        result_list <- c(result_list, list(result))
    }

    dplyr::bind_rows(!!!result_list)
}
