
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
Model.cfg <- R6::R6Class("Model.cfg",
    list(
        model_class = NULL,
        initialize = function() {
        }
    )
)

#' @export
Known.cfg <- R6::R6Class("Known.cfg",
    inherit = Model.cfg,
    list(
        covariate_name = character(),
        model_class = "known",
        initialize = function(covariate_name) {
            self$covariate_name <- covariate_name
        }
    )
)

#' @export
KernelSmooth.cfg <- R6::R6Class("KernelSmooth.cfg",
    inherit = Model.cfg,
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
    inherit = Model.cfg,
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
    inherit = Model.cfg,
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
        estimand = "PCATE",
        initialize = function(covariates, effect_cfg, cfgs) {
            # cfgs is a named list from covariate name to a model config
            # this is exactly like the MCATE
            self$cfgs <- cfgs
            # a vector of covariate names that should be fed into the joint effect model
            self$model_covariates <- covariates
            self$effect_cfg <- effect_cfg
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