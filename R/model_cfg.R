
#' @export
SL.Learner.cfg <- R6::R6Class("SL.Learner.cfg",
    list(
        model_name = character(),
        hyperparameters = NULL,
        initialize = function(model_name, hp=NULL) {
            self$model_name <- model_name
            self$hyperparameters <- hp
        }
    )
)

#' @export
Model_cfg <- R6::R6Class("Model_cfg",
    list(
        model_class = NULL,
        initialize = function() {
        }
    )
)

#' @export
Known_cfg <- R6::R6Class("Known_cfg",
    inherit = Model_cfg,
    list(
        covariate_name = character(),
        model_class = "known",
        initialize = function(covariate_name) {
            self$covariate_name <- covariate_name
        }
    )
)

#' @export
KernelSmooth_cfg <- R6::R6Class("KernelSmooth_cfg",
    inherit = Model_cfg,
    list(
        model_class = "KernelSmooth",
        neval = integer(),
        initialize = function(neval = 100) {
            self$neval <- neval
        }
    )
)


#' @export
Stratified.cfg <- R6::R6Class("Stratified.cfg",
    inherit = Model_cfg,
    public = list(
        model_class = "Stratified",
        covariate = character(),
        initialize = function(covariate) {
            self$covariate <- covariate
        }
    )
)

#' @export
SL.cfg <- R6::R6Class("SL.cfg",
    inherit = Model_cfg,
    public = list(
        cvControl = list(V = 10),
        SL.library = character(),
        SL.env = NULL,
        model_class = "SL",
        initialize = function(cvControl = NULL, learner_cfgs = NULL) {
            if (!is.null(cvControl)) {
                self$cvControl <- cvControl
            }
            if (is.null(learner_cfgs)) {
                learner_cfgs <- list(SL.Learner.cfg$new("SL.glm"))
            }
            sl_lib <- character()
            self$SL.env <- new.env()
            for (lrnr in learner_cfgs) {
                learner_name <- lrnr$model_name
                hyperparameters <- lrnr$hyperparameters
                if (is.null(hyperparameters)) {
                    lrnrs <- learner_name
                } else {
                    learners <- create.Learner(
                        learner_name,
                        tune = hyperparameters,
                        detailed_names = TRUE,
                        name_prefix = paste0("custom_", learner_name),
                        env = self$SL.env
                    )
                    lrnrs <- learners$names
                }
                sl_lib <- c(sl_lib, lrnrs)
            }
            self$SL.library <- unique(sl_lib)
        }
    )
)

MCATE.cfg <- R6::R6Class("MCATE.cfg",
    list(
        cfgs = list(),
        std.errors = logical(),
        estimand = "MCATE",
        initialize = function(cfgs, std.errors=TRUE) {
            # cfgs is a named list from covariate name to a model config
            self$cfgs <- cfgs
            self$std.errors <- std.errors
        }
    )
)

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

VIMP.cfg <- R6::R6Class("VIMP.cfg",
    list(
        model_cfg = list(),
        estimand = "VIMP",
        initialize = function(model_cfg) {
            self$model_cfg <- model_cfg
        }
    )
)

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
QoI.cfg <- R6::R6Class("QoI.cfg",
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