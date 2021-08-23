#' @export
MCATE.cfg <- R6::R6Class("MCATE.cfg",
    list(
        cfgs = list(),
        std_errors = logical(),
        estimand = "MCATE",
        initialize = function(cfgs, std_errors=TRUE) {
            # cfgs is a named list from covariate name to a model config
            self$cfgs <- cfgs
            self$std_errors <- std_errors
        }
    )
)

#' @export
PCATE.cfg <- R6::R6Class("PCATE.cfg",
    list(
        cfgs = list(),
        effect_cfg = list(),
        model_covariates = character(),
        num_mc_samples = list(),
        estimand = "PCATE",
        initialize = function(model_covariates, effect_cfg, cfgs, num_mc_samples = 100) {
            # cfgs is a named list from covariate name to a model config
            # this is exactly like the MCATE
            self$cfgs <- cfgs
            # a vector of covariate names that should be fed into the joint effect model
            self$model_covariates <- model_covariates
            self$effect_cfg <- effect_cfg
            if (is.atomic(num_mc_samples) && (num_mc_samples == trunc(num_mc_samples))) {
                self$num_mc_samples <- as.list(
                    structure(rep(num_mc_samples, length(cfgs)), names = names(cfgs))
                )
            } else if (is.list(num_mc_samples)) {
                self$num_mc_samples <- num_mc_samples
            } else {
                stop("Unknown type of num_mc_samples")
            }
        }
    )
)

#' @export
VIMP.cfg <- R6::R6Class("VIMP.cfg",
    list(
        model_cfg = list(),
        estimand = "VIMP",
        initialize = function(model_cfg) {
            self$model_cfg <- model_cfg
        }
    )
)

#' @export
Diagnostics.cfg <- R6::R6Class("Diagnostics.cfg",
    list(
        ps = character(),
        outcome = character(),
        effect = character(),
        initialize = function(ps = NULL, outcome = NULL, effect = NULL) {
            if (!is.null(ps)) self$ps <- ps
            if (!is.null(outcome)) self$outcome <- outcome
            if (!is.null(effect)) self$effect <- effect
        }
    )
)

#' @export
QoI_cfg <- R6::R6Class("QoI_cfg",
    list(
        mcate = NULL,
        pcate = NULL,
        vimp = NULL,
        diag = NULL,
        initialize = function(
            mcate = NULL, pcate = NULL, vimp = NULL, diag = NULL
        ) {
            if (is.null(mcate) && is.null(pcate) && is.null(vimp) && is.null(diag)) {
                stop("Must define at least one QoI!")
            }
            if (!is.null(mcate)) self$mcate <- mcate
            if (!is.null(pcate)) self$pcate <- pcate
            if (!is.null(vimp)) self$vimp <- vimp
            if (!is.null(diag)) self$diag <- diag
        }
    )
)


#' @export
HTE_cfg <- R6::R6Class("HTE_cfg",
    list(
        outcome = list(),
        treatment = list(),
        qoi = list(),
        initialize = function(
            outcome=NULL, treatment=NULL, qoi=NULL
        ) {
            if (is.null(outcome)) outcome <- SLEnsemble_cfg$new()
            if (is.null(treatment)) treatment <- SLEnsemble_cfg$new()
            if (is.null(qoi)) qoi <- QoI_cfg$new()
            self$outcome <- outcome
            self$treatment <- treatment
            self$qoi <- qoi
        }
    )
)
