#' Calculate Variable Importance of HTEs
#'
#' `calculate_vimp` estimates the reduction in (population) $R^2$ from
#' removing a particular moderator from a model containing all moderators.
#' @param full_data dataframe
#' @param weight_col Unquoted name of the weight column.
#' @param pseudo_outcome Unquoted name of the pseudo-outcome.
#' @param ... Unquoted names of covariates to include in the joint effect model.
#' The variable importance will be calculated for each of these covariates.
#' @param .VIMP_cfg A `VIMP_cfg` object defining how VIMP should be estimated.
#' @param .Model_cfg A `Model_cfg` object defining how the joint effect model should be estimated.
#' @references
#' * Williamson, B. D., Gilbert, P. B., Carone, M., & Simon, N. (2021).
#' Nonparametric variable importance assessment using machine learning techniques.
#' Biometrics, 77(1), 9-22.
#' * Williamson, B. D., Gilbert, P. B., Simon, N. R., & Carone, M. (2021).
#' A general framework for inference on algorithm-agnostic variable importance.
#' Journal of the American Statistical Association, 1-14.
#' @seealso [calculate_linear_vimp()]
#' @importFrom progress progress_bar
#' @import SuperLearner
#' @keywords internal
calculate_vimp <- function(full_data, weight_col, pseudo_outcome, ..., .VIMP_cfg, .Model_cfg) {
    dots <- rlang::enexprs(...)
    weight_col <- rlang::enexpr(weight_col)
    pseudo_outcome <- rlang::enexpr(pseudo_outcome)

    check_splits(full_data)

    split_ids <- as.integer(as.factor(full_data[[".split_id"]]))
    unq_splits <- unique(split_ids)
    num_splits_in_data <- length(unq_splits)
    num_splits_in_attr <- attr(full_data, "num_splits")
    if (num_splits_in_data != num_splits_in_attr) stop("Number of splits is inconsistent.")
    if (ceiling(num_splits_in_data / 2) != floor(num_splits_in_data / 2)) {
        stop("Number of splits must be even to calculate VIMP.")
    }

    sample_splitting <- .VIMP_cfg$sample_splitting

    data <- Model_data$new(full_data, {{ pseudo_outcome }}, !!!dots, .weight_col = {{ weight_col }})

    result_list <- list()
    cv_ctl <- data$SL_cv_control()
    if (sample_splitting) {
        inner_cv_ctl <- list(list(V = as.integer(num_splits_in_data / 2)))
    } else {
        inner_cv_ctl <- list()
    }

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
        SL.library = .Model_cfg$SL.library,
        env = .Model_cfg$SL.env,
        cvControl = cv_ctl,
        innerCvControl = inner_cv_ctl,
        obsWeights = data$weights
    ), "Only a single innerCvControl is given", "rank-deficient fit", "(i.e. given weight 0)")

    if (sample_splitting) {
        ss_folds <- vimp::make_folds(unq_splits, V = 2)
    } else {
        ss_folds <- rep(1, length(unq_splits))
    }
    cv_preds_full <- full_fit$SL.predict

    pb$tick()

    for (covariate in names(data$model_frame)) {
        idx <- which(names(data$model_frame) == covariate)
        muffle_warnings(reduced_fit <- SuperLearner::CV.SuperLearner(
            Y = data$label,
            X = data$model_frame[, -idx, drop = FALSE],
            SL.library = .Model_cfg$SL.library,
            env = .Model_cfg$SL.env,
            cvControl = cv_ctl,
            innerCvControl = inner_cv_ctl,
            obsWeights = data$weights
        ), "Only a single innerCvControl is given", "rank-deficient fit", "(i.e. given weight 0)")

        if (!sample_splitting) {
            ss_folds <- rep(2, length(unq_splits))
        }
        cv_preds_reduced <- reduced_fit$SL.predict
        if (!sample_splitting) {
            ss_folds <- NULL
        }

        # VIMP estimates a censoring model which cannot be disabled,
        # so I just set this model to be a simple mean model.
        vimp_sl_lib <- "SL.mean"

        muffle_warnings({
            result <- vimp::cv_vim(
            Y = data$label,
            indx = idx,
            type = "r_squared",
            cross_fitted_f1 = cv_preds_full,
            cross_fitted_f2 = cv_preds_reduced,
            ipc_weights = data$weights,
            # The estimated quantities based on the use of "Y" (censoring related to the outcome)
            # is not actually used because all of the coarsening variables (C) are equal to one.
            Z = "Y",
            SL.library = vimp_sl_lib,
            env = .Model_cfg$SL.env,
            cross_fitting_folds = split_ids,
            sample_splitting_folds = ss_folds,
            run_regression = FALSE,
            V = ifelse(sample_splitting, as.integer(num_splits_in_data / 2), num_splits_in_data),
            scale_est = TRUE,
            sample_splitting = sample_splitting
        )
        }, "estimate < 0", "rank-deficient fit", "(i.e. given weight 0)",
        "duplicates of previous learners", "All metalearner coefficients are zero",
        "All algorithms have zero weight")

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


#' Calculate Linear Variable Importance of HTEs
#'
#' `calculate_linear_vimp` estimates the linear hypothesis test of removing a particular moderator
#' from a linear model containing all moderators. Unlike `calculate_vimp`, this will only be
#' unbiased and have correct asymptotic coverage rates if the true model is linear. This linear
#' approach is also substantially faster, so may be useful when prototyping an analysis.
#' @param full_data dataframe
#' @param weight_col Unquoted name of the weight column.
#' @param pseudo_outcome Unquoted name of the pseudo-outcome.
#' @param ... Unquoted names of covariates to include in the joint effect model.
#' The variable importance will be calculated for each of these covariates.
#' @param .VIMP_cfg A `VIMP_cfg` object defining how VIMP should be estimated.
#' @param .Model_cfg A `Model_cfg` object defining how the joint effect model should be estimated.
#' @references
#' * Williamson, B. D., Gilbert, P. B., Carone, M., & Simon, N. (2021).
#' Nonparametric variable importance assessment using machine learning techniques.
#' Biometrics, 77(1), 9-22.
#' * Williamson, B. D., Gilbert, P. B., Simon, N. R., & Carone, M. (2021).
#' A general framework for inference on algorithm-agnostic variable importance.
#' Journal of the American Statistical Association, 1-14.
#' @seealso [calculate_vimp()]
#' @importFrom progress progress_bar
#' @importFrom stats lm residuals weighted.mean
#' @importFrom dplyr select %>% tibble
#' @importFrom rlang enexprs enexpr
#' @import SuperLearner
#' @keywords internal
calculate_linear_vimp <- function(
    full_data, weight_col, pseudo_outcome, ..., .VIMP_cfg, .Model_cfg
) {
    dots <- rlang::enexprs(...)
    weight_col <- rlang::enexpr(weight_col)
    pseudo_outcome <- rlang::enexpr(pseudo_outcome)

    data <- Model_data$new(full_data, {{ pseudo_outcome }}, !!!dots, .weight_col = {{ weight_col }})

    df <- dplyr::bind_cols(
        .label = data$label,
        data$model_frame
    )

    full_mod <- stats::lm(".label ~ .", df, weights = data$weights)
    r_0 <- stats::residuals(full_mod) ^ 2

    result_list <- list()
    for (moderator in names(data$model_frame)) {
        reduced_mod <- stats::lm(
            ".label ~ .",
            df %>% dplyr::select(-!!moderator),
            weights = data$weights
        )
        r_1 <- stats::residuals(reduced_mod) ^ 2

        diff <- r_1 - r_0
        result <- dplyr::tibble(
            estimand = "VIMP",
            term = moderator,
            estimate = stats::weighted.mean(diff, data$weights),
            std_error = clustered_se_of_mean(diff, data$cluster, data$weights)
        )
        result_list <- c(result_list, list(result))
    }
    dplyr::bind_rows(!!!result_list)
}
