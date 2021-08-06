
#' @export
construct.pseudo.outcomes <- function(.data, y_col, a_col) {
    # check that plugins are in the df
    YA <- unlist(dplyr::select(.data, {{ y_col }}))
    A <- unlist(dplyr::select(.data, {{ a_col }}))
    mu0 <- .data[[".mu0_hat"]]
    mu1 <- .data[[".mu1_hat"]]
    pi <- .data[[".pi_hat"]]
    muA <- A * mu1 + (1 - A) * mu0
    .data$.pseudo.outcome <- (A - pi) / (pi * (1 - pi)) * (YA - muA) + mu1 - mu0
    attr(.data, "treatment") <- rlang::as_string(a_col)
    attr(.data, "outcome") <- rlang::as_string(y_col)
    .data
}

#' @export
calculate.quantities <- function(.data, .outcome, ..., .MCATE.cfg) {
    dots <- rlang::enexprs(...)
    result_list <- list()
    for (covariate in dots) {
        .Model.cfg <- .MCATE.cfg$cfgs[[rlang::as_string(covariate)]]
        data <- Model.data$new(.data, {{ .outcome }}, {{ covariate }})
        predictor <- predictor_factory(.Model.cfg)
        model <- predictor$fit(data)
        if (.MCATE.cfg$std.errors) {
            result <- model$predict_se(data)
            result$term <- rlang::quo_name(rlang::enquo(covariate))
            result <- dplyr::select(
                result, term, x, estimate, std.error
            )
        } else {
            result <- dplyr::tibble(
                term = quo_name(enquo(covariate)),
                x = drop(data$features),
                estimate = model$predict(data)
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
    }

    dplyr::bind_rows(!!!result_list)
}

SL_model_slot <- function(prediction) {
    if (prediction == ".pi_hat") "pi"
    else if (prediction == ".mu1_hat") "mu1"
    else if (prediction == ".mu0_hat") "mu0"
    else stop("Unknown model slot.")
}

#' @export
#' @importFrom pROC auc
diagnostic_factory <- function(.data, label, prediction, diag_name) {
    if (tolower(diag_name) == "auc") {
        n1 <- sum(.data[[label]])
        result <- pROC::auc(.data[[label]], .data[[prediction]])
        result <- dplyr::tibble(
            estimand = "AUC",
            term = label,
            estimate = result,
            # Hanley and McNeil (1982) bound on the variance of AUC
            std.error = 1 / 2 / sqrt(pmin(n1, length(.data[[label]]) - n1))
        )
    } else if (tolower(diag_name) == "mse") {
        sqerr <- (.data[[label]] - .data[[prediction]]) ^ 2
        result <- mean(sqerr)
        stderr <- sd(sqerr) / sqrt(length(sqerr))
        result <- dplyr::tibble(
            estimand = "MSE",
            term = label,
            estimate = result,
            std.error = stderr
        )
    } else if (tolower(diag_name) == "sl_coefs") {
        result_list <- attr(.data, "SL_coefs")[[SL_model_slot(prediction)]]
        result <- dplyr::bind_rows(!!!result_list) %>%
            dplyr::group_by(model_name) %>%
            dplyr::summarize(
                estimate = mean(coef),
                std.error = sd(coef) / sqrt(n()),
                estimand = "SL coefficient"
            ) %>%
            dplyr::rename(term = model_name)
    } else if (tolower(diag_name) == "sl_risk") {
        result_list <- attr(.data, "SL_coefs")[[SL_model_slot(prediction)]]
        result <- dplyr::bind_rows(!!!result_list) %>%
            dplyr::group_by(model_name) %>%
            dplyr::summarize(
                estimate = mean(cvRisk),
                std.error = sd(cvRisk) / sqrt(n()),
                estimand = "SL risk"
            ) %>%
            dplyr::rename(term = model_name)
    }
    result
}

#' @export
calculate_diagnostics <- function(.data, treatment, outcome, .diag.cfg) {
    ps_cfg <- .diag.cfg$ps
    y_cfg <- .diag.cfg$outcome
    fx_cfg <- .diag.cfg$effect

    treatment_name <- attr(.data, "treatment")
    outcome_name <- attr(.data, "outcome")

    result_list <- list()
    for (diag in ps_cfg) {
        result <- diagnostic_factory(.data, treatment_name, ".pi_hat", diag)
        result_list <- c(result_list, list(result))
    }

    for (diag in y_cfg) {
        result1 <- diagnostic_factory(.data %>% filter(.data[[treatment_name]] == 1), outcome_name, ".mu1_hat", diag)
        result1$level <- "Treatment"
        result0 <- diagnostic_factory(.data %>% filter(.data[[treatment_name]] == 0), outcome_name, ".mu0_hat", diag)
        result0$level <- "Control"
        result_list <- c(result_list, list(result0), list(result1))
    }

    for (diag in fx_cfg) {
        result <- diagnostic_factory(.data, diag)
        result_list <- c(result_list, list(result))
    }

    dplyr::bind_rows(!!!result_list)
}

#' @export
calculate.vimp <- function(.data, .outcome, ..., .VIMP.cfg) {
    dots <- rlang::enexprs(...)

    data <- Model.data$new(.data, {{ .outcome }}, !!!dots)

    full_model <- predictor_factory(.VIMP.cfg$model_cfg)
    full_model <- full_model$fit(data)
    full_model_predictions <- drop(full_model$model$SL.predict)

    folds_full <- rep(NA_integer_, length(full_model_predictions))
    idx <- 1
    for (fold in full_model$model$validRows) {
        folds_full[fold] <- idx
        idx <- idx + 1
    }

    result_list <- list()
    idx <- 1
    for (covariate in dots) {
        dots_reduced <- purrr::discard(dots, ~ .x == covariate)
        reduced_data <- Model.data$new(.data, {{ .outcome }}, !!!dots_reduced)
        reduced_model <- SuperLearner::SuperLearner(
                Y = reduced_data$label, X = reduced_data$model_frame, family = full_model$family,
                SL.library = full_model$SL.library, env = full_model$SL.env, 
                cvControl = SuperLearner::SuperLearner.CV.control(
                    V = length(full_model$model$validRows), validRows = full_model$model$validRows
                )
        )
        reduced_model_predictions <- drop(reduced_model$SL.predict)

        result <- vimp::vim(
            Y = data$label,
            f1 = full_model_predictions,
            f2 = reduced_model_predictions,
            indx = idx,
            sample_splitting_folds = folds_full,
            type = "r_squared",
            run_regression = FALSE,
            sample_splitting = FALSE
        )
        # result <- vimp::vim(
        #     Y = data$label,
        #     X = data$model_frame,
        #     indx = idx,
        #     run_regression = TRUE,
        #     SL.library = .VIMP.cfg$model_cfg$SL.library,
        #     env = .VIMP.cfg$model_cfg$SL.env,
        #     type = "r_squared",
        #     sample_splitting = TRUE
        # )
        idx <- idx + 1
        result <- dplyr::tibble(
            estimand = "VIMP",
            term = rlang::quo_name(rlang::enquo(covariate)),
            estimate = result$est,
            std.error = result$se
        )
        result_list <- c(result_list, list(result))
    }
    dplyr::bind_rows(!!!result_list)
}

#' @export
estimate.QoI <- function(
    .data, ..., .HTE.cfg=NULL
) {
    dots <- rlang::enexprs(...)
    if (is.null(.HTE.cfg)) .HTE.cfg <- .HTE.cfg$new()

    .QoI.cfg <- .HTE.cfg$qoi
    result_list <- list()

    if (!is.null(.QoI.cfg$diag)) {
        result <- calculate_diagnostics(.data, .diag.cfg = .QoI.cfg$diag)
        result_list <- c(result_list, list(result))
    }

    if (!is.null(.QoI.cfg$mcate)) {
        result <- calculate.quantities(.data, .pseudo.outcome, !!!dots, .MCATE.cfg = .QoI.cfg$mcate)
        result_list <- c(result_list, list(dplyr::mutate(result, estimand = "MCATE")))
    }

    if (!is.null(.QoI.cfg$vimp)) {
        result <- calculate.vimp(.data, .pseudo.outcome, !!!dots, .VIMP.cfg = .QoI.cfg$vimp)
        result_list <- c(result_list, list(result))
    }

    if (!is.null(.QoI.cfg$pcate)) {
        stop("Not implemented.")
    }

    col_order <- c("estimand", "term", "value", "level", "estimate", "std.error")

    dplyr::bind_rows(!!!result_list) %>%
        dplyr::select(intersect(col_order, names(.)))
}