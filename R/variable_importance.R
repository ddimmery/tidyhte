#' @export
calculate_vimp <- function(.data, .outcome, ..., .VIMP.cfg) {
    dots <- rlang::enexprs(...)

    data <- Model_data$new(.data, {{ .outcome }}, !!!dots)

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
        reduced_data <- Model_data$new(.data, {{ .outcome }}, !!!dots_reduced)
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
            std_error = result$se
        )
        result_list <- c(result_list, list(result))
    }
    dplyr::bind_rows(!!!result_list)
}
