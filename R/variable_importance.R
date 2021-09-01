#' Calculate Variable Importance of HTEs
#'
#' `calculate_vimp` estimates the reduction in (population) $R^2$ from removing a particular moderator
#' from a model containing all moderators.
#' @param .data dataframe
#' @param .outcome Unquoted name of the pseudo-outcome.
#' @param ... Unquoted names of covariates to include in the joint effect model. The variable importance
#' will be calculated for each of these covariates.
#' @param .VIMP_cfg A `VIMP_cfg` object defining how the joint effect model should be estimated.
#' @references Williamson, BD, Gilbert, PB, Carone, M, Simon, N. Nonparametric variable importance
#' assessment using machine learning techniques. *Biometrics*. 2021; 77: 9-- 22.
#' [https://doi.org/10.1111/biom.13392](https://doi.org/10.1111/biom.13392)
#' @importFrom progress progress_bar
#' @export
calculate_vimp <- function(.data, pseudo_outcome, ..., .VIMP_cfg) {
    dots <- rlang::enexprs(...)

    data <- Model_data$new(.data, {{ pseudo_outcome }}, !!!dots)

    # full_model <- predictor_factory(.VIMP_cfg$model_cfg)
    # full_model <- full_model$fit(data)
    # full_model_predictions <- drop(full_model$model$SL.predict)

    result_list <- list()
    idx <- 1
    cv_ctl <- data$SL_cv_control()

    pb <- progress::progress_bar$new(
        total = ncol(data$model_frame),
        format = "estimating VIMP [:bar] covariates: :current / :total"
    )
    pb$tick(0)
    for (covariate in names(data$model_frame)) {
        # dots_reduced <- purrr::discard(dots, ~ .x == covariate)
        # reduced_data <- Model_data$new(.data, {{ .outcome }}, !!!dots_reduced)
        # reduced_model <- SuperLearner::SuperLearner(
        #         Y = reduced_data$label, X = reduced_data$model_frame, family = full_model$family,
        #         SL.library = full_model$SL.library, env = full_model$SL.env,
        #         cvControl = data$SL_cv_control()
        # )
        # reduced_model_predictions <- drop(reduced_model$SL.predict)
        muffle_warnings({
            result <- vimp::cv_vim(
            Y = data$label,
            X = data$model_frame,
            indx = idx,
            run_regression = TRUE,
            SL.library = .VIMP_cfg$model_cfg$SL.library,
            env = .VIMP_cfg$model_cfg$SL.env,
            cvControl = list(V = 2 * cv_ctl$V),
            V = cv_ctl$V,
            sample_splitting = TRUE
        )
        }, "estimate < 0", "rank-deficient fit")
        # print(covariate)
        # print(result)
        # result <- vimp::vim(
        #     Y = data$label,
        #     f1 = full_model_predictions,
        #     f2 = reduced_model_predictions,
        #     indx = idx,
        #     sample_splitting_folds = .data$.split_id,
        #     type = "r_squared",
        #     run_regression = FALSE,
        #     sample_splitting = FALSE
        # )
        idx <- idx + 1
        result <- dplyr::tibble(
            estimand = "VIMP",
            term = covariate,
            estimate = result$est,
            std_error = result$se
        )
        result_list <- c(result_list, list(result))
        pb$tick()
    }
    dplyr::bind_rows(!!!result_list)
}
