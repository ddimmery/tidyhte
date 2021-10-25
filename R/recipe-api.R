#' Create a basic config for HTE estimation
#'
#' This provides a basic recipe for HTE estimation that can
#' be extended by providing additional information about models
#' to be estimated and what quantities of interest should be
#' returned based on those models. This basic model includes
#' only linear models for nuisance function estimation.
#' @return `HTE_cfg` object
#' @export
basic_config <- function() {
    trt_cfg <- SLEnsemble_cfg$new()
    reg_cfg <- SLEnsemble_cfg$new()
    fx_cfg <- SLEnsemble_cfg$new()
    qoi_cfg <- QoI_cfg$new(
        ate = TRUE,
        vimp = VIMP_cfg$new(sample_splitting = TRUE),
        diag = Diagnostics_cfg$new(
            ps = c("AUC", "MSE", "SL_risk", "SL_coefs"),
            outcome = c("MSE", "SL_risk", "SL_coefs"),
            effect = c("MSE", "SL_risk", "SL_coefs")
        )
    )
    hte_cfg <- HTE_cfg$new(
        treatment = trt_cfg,
        outcome = reg_cfg,
        effect = fx_cfg,
        qoi = qoi_cfg
    )
    invisible(hte_cfg)
}

#' Add an additional model to the propensity score ensemble
#'
#' This adds a learner to the ensemble used for estimating propensity
#' scores.
#' @param hte_cfg `HTE_cfg` object to update.
#' @param model_name Character indicating the name of the model to
#' incorporate into the propensity score ensemble. Possible values
#' use `SuperLearner` naming conventions. A full list is available
#' with `SuperLearner::listWrappers("SL")`
#' @param ... Parameters over which to grid-search for this model class.
#' @return Updated `HTE_cfg` object
#' @export
add_propensity_score_model <- function(hte_cfg, model_name, ...) {
    hps <- rlang::dots_list(..., .named = TRUE)
    if (length(hps) == 0) hps <- NULL
    if (!checkmate::test_r6(hte_cfg$treatment, classes = "SLEnsemble_cfg")) {
        hte_cfg$treatment <- SLEnsemble_cfg$new()
    }
    hte_cfg$treatment$add_sublearner(model_name, hps)
    invisible(hte_cfg)
}

#' Uses a known propensity score
#'
#' This replaces the propensity score model with a known value
#' of the propensity score.
#' @param hte_cfg `HTE_cfg` object to update.
#' @param covariate_name Character indicating the name of the covariate
#' name in the dataframe corresponding to the known propensity score.
#' @return Updated `HTE_cfg` object
#' @export
add_known_propensity_score <- function(hte_cfg, covariate_name) {
    hte_cfg$treatment <- Known_cfg$new(covariate_name)
    invisible(hte_cfg)
}

#' Add an additional diagnostic to the propensity score
#'
#' This adds a diagnostic to the propensity score.
#' @param hte_cfg `HTE_cfg` object to update.
#' @param diag Character indicating the name of the diagnostic
#' to include. Possible values are `"MSE"`, `"AUC"` and, for
#' `SuperLearner` ensembles, `"SL_risk"` and `"SL_coefs"`.
#' @return Updated `HTE_cfg` object
#' @export
add_propensity_diagnostic <- function(hte_cfg, diag) {
    hte_cfg$qoi$diag$add(ps = diag)
    invisible(hte_cfg)
}

#' Add an additional model to the outcome ensemble
#'
#' This adds a learner to the ensemble used for estimating a model
#' of the conditional expectation of the outcome.
#' @param hte_cfg `HTE_cfg` object to update.
#' @param model_name Character indicating the name of the model to
#' incorporate into the outcome ensemble. Possible values
#' use `SuperLearner` naming conventions. A full list is available
#' with `SuperLearner::listWrappers("SL")`
#' @param ... Parameters over which to grid-search for this model class.
#' @return Updated `HTE_cfg` object
#' @export
add_outcome_model <- function(hte_cfg, model_name, ...) {
    hps <- rlang::dots_list(..., .named = TRUE)
    if (length(hps) == 0) hps <- NULL
    if (!checkmate::test_r6(hte_cfg$outcome, classes = "SLEnsemble_cfg")) {
        hte_cfg$outcome <- SLEnsemble_cfg$new()
    }
    hte_cfg$outcome$add_sublearner(model_name, hps)
    invisible(hte_cfg)
}

#' Add an additional diagnostic to the outcome model
#'
#' This adds a diagnostic to the outcome model.
#' @param hte_cfg `HTE_cfg` object to update.
#' @param diag Character indicating the name of the diagnostic
#' to include. Possible values are `"MSE"` and, for
#' `SuperLearner` ensembles, `"SL_risk"` and `"SL_coefs"`.
#' @return Updated `HTE_cfg` object
#' @export
add_outcome_diagnostic <- function(hte_cfg, diag) {
    hte_cfg$qoi$diag$add(outcome = diag)
    invisible(hte_cfg)
}

#' Add an additional model to the joint effect ensemble
#'
#' This adds a learner to the ensemble used for estimating a model
#' of the conditional expectation of the pseudo-outcome.
#' @param hte_cfg `HTE_cfg` object to update.
#' @param model_name Character indicating the name of the model to
#' incorporate into the joint effect ensemble. Possible values
#' use `SuperLearner` naming conventions. A full list is available
#' with `SuperLearner::listWrappers("SL")`
#' @param ... Parameters over which to grid-search for this model class.
#' @return Updated `HTE_cfg` object
#' @export
add_effect_model <- function(hte_cfg, model_name, ...) {
    hps <- rlang::dots_list(..., .named = TRUE)
    if (length(hps) == 0) hps <- NULL
    hte_cfg$effect$add_sublearner(model_name, hps)
    invisible(hte_cfg)
}

#' Add an additional diagnostic to the effect model
#'
#' This adds a diagnostic to the effect model.
#' @param hte_cfg `HTE_cfg` object to update.
#' @param diag Character indicating the name of the diagnostic
#' to include. Possible values are `"MSE"` and, for
#' `SuperLearner` ensembles, `"SL_risk"` and `"SL_coefs"`.
#' @return Updated `HTE_cfg` object
#' @export
add_effect_diagnostic <- function(hte_cfg, diag) {
    hte_cfg$qoi$diag$add(effect = diag)
    invisible(hte_cfg)
}

#' Adds moderators to the configuration
#'
#' This adds a definition about how to display a moderators to
#' the MCATE config.
#' @param hte_cfg `HTE_cfg` object to update.
#' @param model_type Character indicating the model type for these moderators.
#' Currently two model types are supported: `"Stratified"` for discrete moderators
#' and `"KernelSmooth"` for continuous ones.
#' @param ... The (unquoted) names of the moderator variables.
#' @param .model_arguments A named list from argument name to value to pass into the
#' constructor for the model. See `Stratified_cfg` and `KernelSmooth_cfg` for more details.
#' @return Updated `HTE_cfg` object
#' @export
add_moderator <- function(hte_cfg, model_type, ..., .model_arguments = NULL) {
    moderators <- rlang::enexprs(...)

    discrete <- tolower(model_type) == "stratified"
    if (discrete) {
        model_cls <- Stratified_cfg
    } else if (tolower(model_type) == "kernelsmooth") {
        model_cls <- KernelSmooth_cfg
    } else {
        stop("Unknown `model_type`.")
    }

    qoi_list <- rlang::list2()
    for (moderator in moderators) {
        mod_name <- rlang::as_name(moderator)
        if (discrete) {
            qoi_list[[mod_name]] <- model_cls$new(covariate = mod_name)
        } else {
            if (is.null(.model_arguments)) .model_arguments <- list()
            qoi_list[[mod_name]] <- do.call(model_cls$new, .model_arguments)
        }
    }

    if (is.null(hte_cfg$qoi$mcate)) {
        hte_cfg$qoi$mcate <- MCATE_cfg$new(qoi_list)
    } else {
        for (var in names(qoi_list)) {
            hte_cfg$qoi$mcate$add_moderator(var, qoi_list[[var]])
        }
    }
    invisible(hte_cfg)
}

#' Adds variable importance information
#'
#' This adds a variable importance quantity of interest to the outputs.
#' @param hte_cfg `HTE_cfg` object to update.
#' @param sample_splitting Logical indicating whether to use sample splitting or not.
#' Choosing not to use sample splitting means that inference will only be valid for
#' moderators with non-null importance.
#' @return Updated `HTE_cfg` object
#' @export
add_vimp <- function(hte_cfg, sample_splitting = TRUE) {
    hte_cfg$qoi$vimp <- VIMP_cfg$new(
        sample_splitting = sample_splitting
    )
    invisible(hte_cfg)
}