#' Calculate Variable Importance of HTEs
#'
#' `calculate_vimp` estimates the reduction in (population) $R^2$ from removing a particular moderator
#' from a model containing all moderators.
#' @param .data dataframe
#' @param pseudo_outcome Unquoted name of the pseudo-outcome.
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

    ident_name <- attr(.data, "identifier")
    ident <- rlang::ensym(ident_name)
    if (package_present("quickblock")) {
        .data <- make_splits(.data, {{ ident }}, !!!dots, .num_splits = .VIMP_cfg$num_splits)
    } else {
        message("`quickblock` is not installed, so falling back to un-stratified CV for VIMP.")
        .data <- make_splits(.data, {{ ident }}, .num_splits = .VIMP_cfg$num_splits)
    }

    data <- Model_data$new(.data, {{ pseudo_outcome }}, !!!dots)

    result_list <- list()
    idx <- 1
    cv_ctl <- data$SL_cv_control()

    pb <- progress::progress_bar$new(
        total = ncol(data$model_frame),
        show_after = 0,
        format = "estimating VIMP [:bar] covariates: :current / :total"
    )
    pb$tick(0)
    for (covariate in names(data$model_frame)) {
        muffle_warnings({
            result <- vimp::cv_vim(
            Y = data$label,
            X = data$model_frame,
            indx = idx,
            run_regression = TRUE,
            SL.library = .VIMP_cfg$model_cfg$SL.library,
            env = .VIMP_cfg$model_cfg$SL.env,
            cvControl = cv_ctl,
            V = as.integer(cv_ctl$V / 2),
            sample_splitting = TRUE
        )
        }, "estimate < 0", "rank-deficient fit")
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
