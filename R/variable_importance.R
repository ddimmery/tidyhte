#' Calculate Variable Importance of HTEs
#'
#' `calculate_vimp` estimates the reduction in (population) $R^2$ from removing a particular moderator
#' from a model containing all moderators.
#' @param .data dataframe
#' @param weight_col Unquoted name of the weight column.
#' @param pseudo_outcome Unquoted name of the pseudo-outcome.
#' @param ... Unquoted names of covariates to include in the joint effect model. The variable importance
#' will be calculated for each of these covariates.
#' @param .VIMP_cfg A `VIMP_cfg` object defining how the joint effect model should be estimated.
#' @references Williamson, BD, Gilbert, PB, Carone, M, Simon, N. Nonparametric variable importance
#' assessment using machine learning techniques. *Biometrics*. 2021; 77: 9-- 22.
#' \doi{10.1111/biom.13392}
#' @importFrom progress progress_bar
#' @import SuperLearner
calculate_vimp <- function(.data, weight_col, pseudo_outcome, ..., .VIMP_cfg) {
    dots <- rlang::enexprs(...)
    weight_col <- rlang::enexpr(weight_col)
    pseudo_outcome <- rlang::enexpr(pseudo_outcome)

    check_splits(.data)

    num_splits_in_data <- length(unique(.data[[".split_id"]]))
    num_splits_in_attr <- attr(.data, "num_splits")
    if (num_splits_in_data != num_splits_in_attr) stop("Number of splits is inconsistent.")
    if (ceiling(num_splits_in_data / 2) != floor(num_splits_in_data / 2)) {
        stop("Number of splits must be even to calculate VIMP.")
    }

    data <- Model_data$new(.data, {{ pseudo_outcome }}, !!!dots, .weight_col = {{ weight_col }})

    result_list <- list()
    idx <- 1
    cv_ctl <- data$SL_cv_control()

    soft_require("quadprog")

    pb <- progress::progress_bar$new(
        total = 1 + ncol(data$model_frame),
        show_after = 0,
        format = "estimating VIMP [:bar] models: :current / :total"
    )
    pb$tick(0)

    muffle_warnings(full_fit <- SuperLearner::CV.SuperLearner(
        Y = data$label,
        X = data$model_frame,
        SL.library = .VIMP_cfg$model_cfg$SL.library,
        env = .VIMP_cfg$model_cfg$SL.env,
        cvControl = cv_ctl,
        innerCvControl = list(list(V = as.integer(num_splits_in_data / 2))),
        obsWeights = data$weights
    ), "Only a single innerCvControl is given", "rank-deficient fit", "(i.e. given weight 0)")
    pb$tick()

    cross_fitting_folds <- vimp::get_cv_sl_folds(full_fit$folds)
    sample_splitting_folds <- vimp::make_folds(unique(cross_fitting_folds), V = 2)
    full_preds <- vimp::extract_sampled_split_predictions(
        full_fit,
        sample_splitting_folds = sample_splitting_folds,
        full = TRUE
    )

    for (covariate in names(data$model_frame)) {
        muffle_warnings(reduced_fit <- SuperLearner::CV.SuperLearner(
            Y = data$label,
            X = data$model_frame[, -idx, drop = FALSE],
            SL.library = .VIMP_cfg$model_cfg$SL.library,
            env = .VIMP_cfg$model_cfg$SL.env,
            cvControl = cv_ctl,
            innerCvControl = list(list(V = as.integer(num_splits_in_data / 2))),
            obsWeights = data$weights
        ), "Only a single innerCvControl is given", "rank-deficient fit", "(i.e. given weight 0)")
        reduced_preds <- vimp::extract_sampled_split_predictions(
            reduced_fit,
            sample_splitting_folds = sample_splitting_folds,
            full = FALSE
        )

        # VIMP estimates a censoring model which cannot be disabled,
        # so I just set this model to be a simple mean model.
        vimp_sl_lib <- "SL.mean"

        muffle_warnings({
            result <- vimp::cv_vim(
            Y = data$label,
            type = "r_squared",
            cross_fitted_f1 = full_preds,
            cross_fitted_f2 = reduced_preds,
            ipc_weights = data$weights,
            # The estimated quantities based on the use of "Y" (censoring related to the outcome)
            # is not actually used because all of the coarsening variables (C) are equal to one.
            Z = "Y",
            indx = idx,
            SL.library = vimp_sl_lib,
            env = .VIMP_cfg$model_cfg$SL.env,
            cross_fitting_folds = cross_fitting_folds,
            sample_splitting_folds = sample_splitting_folds,
            run_regression = FALSE,
            V = as.integer(num_splits_in_data / 2)
        )
        }, "estimate < 0", "rank-deficient fit", "(i.e. given weight 0)", "duplicates of previous learners")
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
