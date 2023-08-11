#' Attach an `HTE_cfg` to a dataframe
#'
#' This adds a configuration attribute to a dataframe for HTE estimation.
#' This configuration details the full analysis of HTE that should be performed.
#'
#' For information about how to set up an `HTE_cfg` object, see the Recipe API
#' documentation [basic_config()].
#'
#' To see an example analysis, read `vignette("experimental_analysis")` in the context
#' of an experiment, `vignette("experimental_analysis")` for an observational study, or
#' `vignette("methodological_details")` for a deeper dive under the hood.
#' @param data dataframe
#' @param .HTE_cfg `HTE_cfg` object representing the full configuration of the HTE analysis.
#' @seealso [basic_config()], [make_splits()], [produce_plugin_estimates()],
#' [construct_pseudo_outcomes()], [estimate_QoI()]
#' @examples
#' library("dplyr")
#' if(require("palmerpenguins")) {
#' data(package = 'palmerpenguins')
#' penguins$unitid = seq_len(nrow(penguins))
#' penguins$propensity = rep(0.5, nrow(penguins))
#' penguins$treatment = rbinom(nrow(penguins), 1, penguins$propensity)
#' cfg <- basic_config() %>% 
#' add_known_propensity_score("propensity") %>%
#' add_outcome_model("SL.glm.interaction") %>%
#' remove_vimp()
#' attach_config(penguins, cfg) %>%
#' make_splits(unitid, .num_splits = 4) %>%
#' produce_plugin_estimates(outcome = body_mass_g, treatment = treatment, species, sex) %>%
#' construct_pseudo_outcomes(body_mass_g, treatment) %>%
#' estimate_QoI(species, sex)
#' }
#' @export
attach_config <- function(data, .HTE_cfg) {
    check_hte_cfg(.HTE_cfg)

    attr(data, "HTE_cfg") <- .HTE_cfg

    data
}


#' Define splits for cross-fitting
#'
#' This takes a dataset, a column with a unique identifier and an
#' arbitrary number of covariates on which to stratify the splits.
#' It returns the original dataset with an additional column `.split_id`
#' corresponding to an identifier for the split.
#' 
#' To see an example analysis, read `vignette("experimental_analysis")` in the context
#' of an experiment, `vignette("experimental_analysis")` for an observational study, or
#' `vignette("methodological_details")` for a deeper dive under the hood.
#' 
#' @param data dataframe
#' @param identifier Unquoted name of unique identifier column
#' @param ... variables on which to stratify (requires that `quickblock` be installed.)
#' @param .num_splits number of splits to create. If VIMP is requested in `QoI_cfg`, this
#' must be an even number.
#' @return original dataframe with additional `.split_id` column
#' @seealso [attach_config()], [produce_plugin_estimates()], [construct_pseudo_outcomes()],
#' [estimate_QoI()]
#' @examples
#' library("dplyr")
#' if(require("palmerpenguins")) {
#' data(package = 'palmerpenguins')
#' penguins$unitid = seq_len(nrow(penguins))
#' penguins$propensity = rep(0.5, nrow(penguins))
#' penguins$treatment = rbinom(nrow(penguins), 1, penguins$propensity)
#' cfg <- basic_config() %>% 
#' add_known_propensity_score("propensity") %>%
#' add_outcome_model("SL.glm.interaction") %>%
#' remove_vimp()
#' attach_config(penguins, cfg) %>%
#' make_splits(unitid, .num_splits = 4) %>%
#' produce_plugin_estimates(outcome = body_mass_g, treatment = treatment, species, sex) %>%
#' construct_pseudo_outcomes(body_mass_g, treatment) %>%
#' estimate_QoI(species, sex)
#' }
#' @importFrom magrittr %>%
#' @importFrom stats model.matrix
#' @importFrom tibble as_tibble
#' @export
make_splits <- function(data, identifier, ..., .num_splits) {
    dots <- rlang::enexprs(...)
    identifier <- rlang::enexpr(identifier)

    if (! "HTE_cfg" %in% names(attributes(data))) {
        use_vimp <- FALSE
    } else {
        check_data_has_hte_cfg(data)
        use_vimp <- !is.null(attr(data, "HTE_cfg")$qoi$vimp)
    }

    if (use_vimp) {
        V <- .num_splits / 2
        if (!checkmate::test_integerish(V, lower = floor(V), upper = ceiling(V))) {
            message("`num_splits` must be even if VIMP is requested as a QoI. Rounding up.")
            V <- ceiling(V)
        }
        .num_splits <- 2 * V
    }

    check_identifier(data, rlang::as_name(identifier))

    qb_present <- package_present("quickblock")

    if (length(dots) > 0 && !qb_present) {
        message("`quickblock` is not installed, so falling back to un-stratified CV.")
    }

    ok_data <- listwise_deletion(data, !!!dots)

    if (length(dots) > 0 && qb_present) {
        soft_require("quickblock")
        block_data <-  tibble::as_tibble(
            stats::model.matrix(~. + 0, dplyr::select(ok_data, !!!dots))
        )
        block_data$id_col <- ok_data[[rlang::as_name(identifier)]]
        block_data %>%
            dplyr::group_by(.data$id_col) %>%
            dplyr::summarize_all(mean) -> block_data
        ids <- block_data$id_col
        block_data %>%
            dplyr::select(!dplyr::matches("id_col")) %>%
            as.matrix() %>%
            quickblock::quickblock(size_constraint = .num_splits) -> qb
        splits <- quickblock::assign_treatment(qb, treatments = 1:.num_splits)
        split_data <- dplyr::tibble(ids = ids, .split_id = splits)
        names(split_data)[1] <- rlang::as_name(identifier)
    } else {
        num_per_split <- as.integer(
            floor(length(unique(ok_data[[rlang::as_name(identifier)]])) / .num_splits)
        )
        ok_data %>%
        dplyr::group_by({{ identifier }}) %>%
        dplyr::tally() %>%
        dplyr::mutate(
            .split_id = sample(c(
                rep(1:.num_splits, rep(num_per_split, .num_splits)),
                sample(.num_splits, dplyr::n() - .num_splits * num_per_split)
            ))
        ) %>%
        dplyr::select({{ identifier }}, ".split_id") -> split_data
    }

    data <- dplyr::left_join(
        data %>% dplyr::select(!dplyr::starts_with(".split_id")),
        split_data,
        by = rlang::as_name(identifier)
    )

    attr(data, "num_splits") <- .num_splits
    attr(data, "identifier") <- rlang::as_name(identifier)

    data
}

#' Estimate models of nuisance functions
#'
#' This takes a dataset with an identified outcome and treatment column along
#' with any number of covariates and appends three columns to the dataset corresponding
#' to an estimate of the conditional expectation of treatment (`.pi_hat`), along with the
#' conditional expectation of the control and treatment potential outcome surfaces
#' (`.mu0_hat` and `.mu1_hat` respectively).
#' 
#' To see an example analysis, read `vignette("experimental_analysis")` in the context
#' of an experiment, `vignette("experimental_analysis")` for an observational study, or
#' `vignette("methodological_details")` for a deeper dive under the hood.
#' 
#' @param data dataframe (already prepared with `attach_config` and `make_splits`)
#' @param outcome Unquoted name of the outcome variable.
#' @param treatment Unquoted name of the treatment variable.
#' @param ... Unquoted names of covariates to include in the models of the nuisance functions.
#' @param .weights Unquoted name of weights column. If NULL, all analysis will assume weights
#' are all equal to one and sample-based quantities will be returned.
#' @seealso [attach_config()], [make_splits()], [construct_pseudo_outcomes()], [estimate_QoI()]
#' @examples
#' library("dplyr")
#' if(require("palmerpenguins")) {
#' data(package = 'palmerpenguins')
#' penguins$unitid = seq_len(nrow(penguins))
#' penguins$propensity = rep(0.5, nrow(penguins))
#' penguins$treatment = rbinom(nrow(penguins), 1, penguins$propensity)
#' cfg <- basic_config() %>% 
#' add_known_propensity_score("propensity") %>%
#' add_outcome_model("SL.glm.interaction") %>%
#' remove_vimp()
#' attach_config(penguins, cfg) %>%
#' make_splits(unitid, .num_splits = 4) %>%
#' produce_plugin_estimates(outcome = body_mass_g, treatment = treatment, species, sex) %>%
#' construct_pseudo_outcomes(body_mass_g, treatment) %>%
#' estimate_QoI(species, sex)
#' }
#' @importFrom progress progress_bar
#' @importFrom dplyr matches left_join select
#' @export
produce_plugin_estimates <- function(data, outcome, treatment, ..., .weights = NULL) {
    dots <- rlang::enexprs(...)
    outcome <- rlang::enexpr(outcome)
    treatment <- rlang::enexpr(treatment)

    .weights <- rlang::enexpr(.weights)
    if (is.null(.weights)) {
        data[[".weights"]] <- rep(1, nrow(data))
        .weights <- rlang::quo(!! rlang::sym(".weights"))
    }

    check_data_has_hte_cfg(data)
    .HTE_cfg <- attr(data, "HTE_cfg")

    check_splits(data)
    check_weights(data, rlang::as_name(.weights))

    data$.row_id <- seq_len(nrow(data))
    ok_data <- listwise_deletion(data, {{ outcome }}, {{ treatment }}, !!!dots)

    num_splits <- attr(data, "num_splits")
    pi_hat <- rep(NA_real_, nrow(ok_data))
    mu0_hat <- rep(NA_real_, nrow(ok_data))
    mu1_hat <- rep(NA_real_, nrow(ok_data))
    SL_coefs <- list(
        pi = list(),
        mu0 = list(),
        mu1 = list()
    )

    pb <- progress::progress_bar$new(
        total = num_splits,
        show_after = 0,
        format = "estimating nuisance models [:bar] splits: :current / :total",
        force = TRUE
    )
    pb$tick(0)
    for (split_id in seq(num_splits)) {
        folds <- split_data(ok_data, split_id)
        a_model <- fit_plugin_A(
            folds$train, {{ .weights }}, {{ treatment }}, !!!dots,
            .Model_cfg = .HTE_cfg$treatment
        )
        if (.HTE_cfg$treatment$model_class == "SL") {
            SL_coef <- dplyr::tibble(
                split_id = rep(split_id, length(a_model$pi$model$libraryNames)),
                model_name = a_model$pi$model$libraryNames,
                cvRisk = a_model$pi$model$cvRisk,
                coef = a_model$pi$model$coef
            )
            SL_coefs[["pi"]] <- c(SL_coefs[["pi"]], list(SL_coef))
        }

        y_model <- fit_plugin_Y(
            folds$train, {{ .weights }}, {{ outcome }}, {{ treatment }}, !!!dots,
            .Model_cfg = .HTE_cfg$outcome
        )

        if (.HTE_cfg$outcome$model_class == "SL") {
            SL_coef <- dplyr::tibble(
                split_id = rep(split_id, length(y_model$mu0$model$libraryNames)),
                model_name = y_model$mu0$model$libraryNames,
                cvRisk = y_model$mu0$model$cvRisk,
                coef = y_model$mu0$model$coef
            )
            SL_coefs[["mu0"]] <- c(SL_coefs[["mu0"]], list(SL_coef))

            SL_coef <- dplyr::tibble(
                split_id = rep(split_id, length(y_model$mu1$model$libraryNames)),
                model_name = y_model$mu1$model$libraryNames,
                cvRisk = y_model$mu1$model$cvRisk,
                coef = y_model$mu1$model$coef
            )
            SL_coefs[["mu1"]] <- c(SL_coefs[["mu1"]], list(SL_coef))
        }


        if (.HTE_cfg$treatment$model_class == "known") {
            cov <- rlang::sym(.HTE_cfg$treatment$covariate_name)
            dots <- c(dots, cov)
        }
        pred_data <- Model_data$new(folds$holdout, NULL, !!!dots)
        pi_hat[folds$in_holdout] <- a_model$pi$predict(pred_data)$estimate

        if (.HTE_cfg$treatment$model_class == "known") {
            dots <- rlang::enexprs(...)
            pred_data <- Model_data$new(folds$holdout, NULL, !!!dots)
        }

        mu1_hat[folds$in_holdout] <- y_model$mu1$predict(pred_data)$estimate
        mu0_hat[folds$in_holdout] <- y_model$mu0$predict(pred_data)$estimate

        pb$tick()
    }
    ok_data$.pi_hat <- pi_hat
    ok_data$.mu1_hat <- mu1_hat
    ok_data$.mu0_hat <- mu0_hat

    data <- dplyr::left_join(
        data %>% dplyr::select(!dplyr::matches(c(".pi_hat", ".mu1_hat", ".mu0_hat"))),
        ok_data %>% dplyr::select(".row_id", ".pi_hat", ".mu1_hat", ".mu0_hat"),
        by = ".row_id"
    ) %>%
    dplyr::select(!dplyr::matches(".row_id"))

    attr(data, "SL_coefs") <- SL_coefs
    attr(data, "weights") <- rlang::as_name(.weights)
    data
}

#' Estimate Quantities of Interest
#'
#' `estimate_QoI` takes a dataframe already prepared with split IDs,
#' plugin estimates and pseudo-outcomes and calculates the requested
#' quantities of interest (QoIs).
#'
#' To see an example analysis, read `vignette("experimental_analysis")` in the context
#' of an experiment, `vignette("experimental_analysis")` for an observational study, or
#' `vignette("methodological_details")` for a deeper dive under the hood.
#' 
#' @param data data frame (already prepared with `attach_config`, `make_splits`,
#' `produce_plugin_estimates` and `construct_pseudo_outcomes`)
#' @param ... Unquoted names of moderators to calculate QoIs for.
#' @seealso [attach_config()], [make_splits()], [produce_plugin_estimates()],
#' [construct_pseudo_outcomes()],
#' @examples
#' library("dplyr")
#' if(require("palmerpenguins")) {
#' data(package = 'palmerpenguins')
#' penguins$unitid = seq_len(nrow(penguins))
#' penguins$propensity = rep(0.5, nrow(penguins))
#' penguins$treatment = rbinom(nrow(penguins), 1, penguins$propensity)
#' cfg <- basic_config() %>% 
#' add_known_propensity_score("propensity") %>%
#' add_outcome_model("SL.glm.interaction") %>%
#' remove_vimp()
#' attach_config(penguins, cfg) %>%
#' make_splits(unitid, .num_splits = 4) %>%
#' produce_plugin_estimates(outcome = body_mass_g, treatment = treatment, species, sex) %>%
#' construct_pseudo_outcomes(body_mass_g, treatment) %>%
#' estimate_QoI(species, sex)
#' }
#' @export
#' @importFrom rlang .env
estimate_QoI <- function(
    data, ...
) {
    dots <- rlang::enexprs(...)

    check_data_has_hte_cfg(data)
    .HTE_cfg <- attr(data, "HTE_cfg")

    check_splits(data)
    check_nuisance_models(data)
    check_weights(data, attr(data, "weights"))

    if (length(dots) == 0) {
        message("No moderators specified, so pulling list from definitions in QoI.")
        mod_names <- names(.HTE_cfg$qoi$mcate$cfgs)
        dots <- rlang::syms(mod_names)
    }

    nuisance_models <- rlang::syms(c(
        ".pseudo_outcome",
        ".pi_hat",
        ".mu1_hat",
        ".mu0_hat"
    ))
    identifier_name <- attr(data, "identifier")
    outcome_name <- attr(data, "outcome")
    outcome <- rlang::sym(outcome_name)
    treatment_name <- attr(data, "treatment")
    treatment <- rlang::sym(treatment_name)
    weights_name <- attr(data, "weights")
    weights <- rlang::sym(weights_name)

    data <- listwise_deletion(data, {{ outcome }}, {{ treatment }}, !!!dots, !!!nuisance_models)

    .QoI_cfg <- .HTE_cfg$qoi
    result_list <- list()

    if (!is.null(.QoI_cfg$mcate)) {
        result <- calculate_mcate_quantities(
            data,
            {{ weights }},
            ".pseudo_outcome",
            !!!dots,
            .MCATE_cfg = .QoI_cfg$mcate
        )
        result_list <- c(result_list, list(dplyr::mutate(result, estimand = "MCATE")))
    }

    if (!is.null(.QoI_cfg$pcate) || .QoI_cfg$predictions) {
        covs <- rlang::syms(.QoI_cfg$pcate$model_covariates)
        fx_mod <- fit_fx_predictor(
            data,
            {{ weights }},
            ".pseudo_outcome",
            !!!covs,
            .pcate.cfg = .QoI_cfg$pcate,
            .Model_cfg = .HTE_cfg$effect
        )
        data <- fx_mod$data
        ids <- identifier_name
        fx <- data[[".pseudo_outcome_hat"]]
        if (.QoI_cfg$predictions) {
            result <- dplyr::tibble(
                estimand = "Predicted CATE",
                term = outcome_name,
                level = ids,
                estimate = fx,
                std_error = NA_real_
            )
            result_list <- c(result_list, list(result))
        }
    }

    if (!is.null(.QoI_cfg$pcate)) {
        warning("Only use PCATEs if you know what you're doing!")
        result <- calculate_pcate_quantities(
            data,
            {{ weights }},
            ".pseudo_outcome",
            fx_mod$model,
            !!!dots,
            .MCATE_cfg = .QoI_cfg$mcate
        )
        result_list <- c(result_list, list(dplyr::mutate(result, estimand = "PCATE")))
    }

    if (!is.null(.QoI_cfg$vimp)) {
        if (!.QoI_cfg$vimp$linear) {
            result <- calculate_vimp(
                data,
                {{ weights }},
                ".pseudo_outcome",
                !!!dots,
                .VIMP_cfg = .QoI_cfg$vimp,
                .Model_cfg = .HTE_cfg$effect
            )
        } else {
            result <- calculate_linear_vimp(
                data,
                {{ weights }},
                ".pseudo_outcome",
                !!!dots,
                .VIMP_cfg = .QoI_cfg$vimp,
                .Model_cfg = .HTE_cfg$effect
            )
        }
        result_list <- c(result_list, list(result))
    }

    if (!is.null(.QoI_cfg$diag)) {
        result <- calculate_diagnostics(data, .diag.cfg = .QoI_cfg$diag)
        result_list <- c(result_list, list(result))
    }

    if (.QoI_cfg$ate) {
        result <- calculate_ate(data)
        result_list <- c(result_list, list(result))
    }

    col_order <- c("estimand", "term", "value", "level", "estimate", "std_error")

    dplyr::bind_rows(!!!result_list) %>%
        dplyr::select(intersect(col_order, names(.env$.)))
}
