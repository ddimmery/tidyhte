
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
            result$covariate_name <- rlang::quo_name(rlang::enquo(covariate))
            result <- dplyr::select(
                result, covariate_name, x, estimate, std.error
            )
        } else {
            result <- dplyr::tibble(
                covariate_name = quo_name(enquo(covariate)),
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

#' @export 
calculate.vimp <- function(.data, .outcome, ..., .VIMP.cfg) {
    dots <- rlang::enexprs(...)

    data <- Model.data$new(.data, {{ .outcome }}, !!!dots)

    full_model <- predictor_factory(.VIMP.cfg$model_cfg)
    full_model <- full_model$fit(data)
    full_model_predictions <- drop(full_model$model$SL.predict)

    folds_full <- rep(NA_integer_, length(full_model_predictions))
    idx <- 1
    for(fold in full_model$model$validRows) {
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
            covariate_name = rlang::quo_name(rlang::enquo(covariate)),
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

    col_order <- c("estimand", "covariate_name", "value", "level", "estimate", "std.error")

    dplyr::bind_rows(!!!result_list) %>%
        dplyr::select(intersect(col_order, names(.)))
}