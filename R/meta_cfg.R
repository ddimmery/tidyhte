#' Configuration of Marginal CATEs
#'
#' @description
#' `MCATE.cfg` is a configuration class for estimating marginal response surfaces based on heterogeneous
#' treatment effect estimates. "Marginal" in this context implies that all other covariates are marginalized.
#' Thus, if two covariates are highly correlated, it is likely that their MCATE surfaces will be extremely similar.
#' @export
MCATE.cfg <- R6::R6Class("MCATE.cfg",
    public = list(
        #' @field cfgs Named list of covariates names to a `Model_cfg` object defining
        #' how to present that covariate's CATE surface (while marginalizing over all other covariates).
        cfgs = list(),
        #' @field std_errors Boolean indicating whether the results should be returned with standard
        #' errors or not.
        std_errors = logical(),
        #' @field estimand String indicating the estimand to target.
        estimand = "MCATE",

        #' @description
        #' Create a new `MCATE.cfg` object with specified model name and hyperparameters.
        #' @param cfgs Named list from moderator name to a `Model_cfg` object defining how to present
        #' that covariate's CATE surface (while marginalizing over all other covariates)
        #' @param std_errors Boolean indicating whether the results should be returned with standard
        #' errors or not.
        #' @return A new `MCATE.cfg` object.
        #' @examples
        #' MCATE.cfg$new(cfgs = list(x1 = KernelSmooth_cfg$new(neval = 100)))
        initialize = function(cfgs, std_errors=TRUE) {
            self$cfgs <- cfgs
            self$std_errors <- std_errors
        }
    )
)

#' Configuration of Partial CATEs
#'
#' @description
#' `PCATE.cfg` is a configuration class for estimating marginal response surfaces based on heterogeneous
#' treatment effect estimates. "Partial" in this context is used similarly to the use in partial dependence plots
#' or in partial regression. In essence, a PCATE attempts to partial out the contribution to the CATE from all other
#' covariates. Two highly correlated variables may have very different PCATE surfaces.
#' @export
PCATE.cfg <- R6::R6Class("PCATE.cfg",
    list(
        #' @field cfgs Named list of covariates names to a `Model_cfg` object defining
        #' how to present that covariate's CATE surface.
        cfgs = list(),
        #' @field effect_cfg A `Model_cfg` object indicating how to fit the second level effect
        #' regression (joint across all selected covariates).
        effect_cfg = list(),
        #' @field model_covariates A character vector of all the covariates to be included in the second-level
        #' effect regression.
        model_covariates = character(),
        #' @field num_mc_samples A named list from covariate name to the number of Monte Carlo samples to take to
        #' calculate the double integral (See Details).
        num_mc_samples = list(),
        #' @field estimand String indicating the estimand to target.
        estimand = "PCATE",

        #' @description
        #' Create a new `PCATE.cfg` object with specified model name and hyperparameters.
        #' @param model_covariates A character vector of all the covariates to be included in the second-level
        #' effect regression.
        #' @param effect_cfg A `Model_cfg` object indicating how to fit the second level effect
        #' regression (joint across all selected covariates).
        #' @param cfgs Named list from moderator name to a `Model_cfg` object defining how to present
        #' that covariate's CATE surface.
        #' @param num_mc_samples A named list from covariate name to the number of Monte Carlo samples to take to
        #' calculate the double integral (See Details). If all covariates should use the same number of samples,
        #' simply pass the (integer) number of samples.
        #' @return A new `PCATE.cfg` object.
        #' @examples
        #' PCATE.cfg$new(
        #'    cfgs = list(x1 = KernelSmooth_cfg$new(neval = 100)),
        #'    model_covariates = c("x1", "x2", "x3"),
        #'    effect_cfg = SLLearner_cfg$new("SL.glm"),
        #'    num_mc_samples = list(x1 = 100)
        #' )
        initialize = function(model_covariates, effect_cfg, cfgs, num_mc_samples = 100) {
            self$cfgs <- cfgs
            self$model_covariates <- model_covariates
            self$effect_cfg <- effect_cfg
            if (checkmate::test_integerish(num_mc_samples, len = 1)) {
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

#' Configuration of Variable Importance
#'
#' @description
#' `VIMP.cfg` is a configuration class for estimating a variable importance measure across all moderators.
#' This provides a meaningful measure of which moderators explain the most of the CATE surface.
#' @export
VIMP.cfg <- R6::R6Class("VIMP.cfg",
    list(
        #' @field model_cfg A `Model_cfg` object indicating how to fit the second level effect
        #' regression (joint across all moderators).
        model_cfg = list(),
        #' @field estimand String indicating the estimand to target.
        estimand = "VIMP",
        #' @description
        #' Create a new `VIMP.cfg` object with specified model configuration.
        #' @param model_cfg A `Model_cfg` object indicating how to fit the second level effect
        #' regression (joint across all moderators).
        #' @return A new `VIMP.cfg` object.
        #' @examples
        #' VIMP.cfg$new(model_cfg = SLLearner_cfg$new("SL.glm"))
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
