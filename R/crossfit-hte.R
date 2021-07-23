#'
#' @export
produce.plugin.estimates <- function(.data, y_col, a_col, ...,
    .HTE.cfg=NULL
) {
    if (is.null(.HTE.cfg)) .HTE.cfg <- HTE.cfg$new()
    dots <- rlang::enexprs(...)

    num_splits <- max(.data$.split_id)
    pi_hat <- rep(NA_real_, nrow(.data))
    mu0_hat <- rep(NA_real_, nrow(.data))
    mu1_hat <- rep(NA_real_, nrow(.data))
    for (split_id in 1:(num_splits - 1)) {
        folds <- split_data(.data, split_id)

        a_model <- fit.plugin.A(
            folds$train, {{ a_col }}, !!!dots,
            .Model.cfg = .HTE.cfg$treatment
        )

        y_model <- fit.plugin.Y(
            folds$train, {{ y_col }}, {{ a_col }}, !!!dots,
            .Model.cfg = .HTE.cfg$outcome
        )

        if (.HTE.cfg$treatment$model_class == "known") {
            cov <- rlang::sym(.HTE.cfg$treatment$covariate_name)
            dots <- c(dots, cov)
        }
        pred_data <- Model.data$new(folds$holdout, NULL, !!!dots)
        pi_hat[folds$in_holdout] <- a_model$pi$predict(pred_data)

        if (.HTE.cfg$treatment$model_class == "known") {
            dots <- rlang::enexprs(...)
            pred_data <- Model.data$new(folds$holdout, NULL, !!!dots)
        }

        # this might not work
        mu1_hat[folds$in_holdout] <- y_model$mu1$predict(
            pred_data
        )
        mu0_hat[folds$in_holdout] <- y_model$mu0$predict(
            pred_data
        )
    }
    .data$.pi_hat <- pi_hat
    .data$.mu1_hat <- mu1_hat
    .data$.mu0_hat <- mu0_hat
    .data
}


#' @export
HTE.cfg <- R6::R6Class("HTE_cfg",
    list(
        outcome = list(),
        treatment = list(),
        qoi = list(),
        initialize = function(
            outcome=NULL, treatment=NULL, qoi=NULL
        ) {
            if (is.null(outcome)) outcome <- SL.cfg$new()
            if (is.null(treatment)) treatment <- SL.cfg$new()
            if (is.null(qoi)) qoi <- QoI.cfg$new()
            self$outcome <- outcome
            self$treatment <- treatment
            self$qoi <- qoi
        }
    )
)