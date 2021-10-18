#' Configuration of Marginal CATEs
#'
#' @description
#' `MCATE_cfg` is a configuration class for estimating marginal response surfaces based on heterogeneous
#' treatment effect estimates. "Marginal" in this context implies that all other covariates are marginalized.
#' Thus, if two covariates are highly correlated, it is likely that their MCATE surfaces will be extremely similar.
#' @export
MCATE_cfg <- R6::R6Class("MCATE_cfg",
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
        #' Create a new `MCATE_cfg` object with specified model name and hyperparameters.
        #' @param cfgs Named list from moderator name to a `Model_cfg` object defining how to present
        #' that covariate's CATE surface (while marginalizing over all other covariates)
        #' @param std_errors Boolean indicating whether the results should be returned with standard
        #' errors or not.
        #' @return A new `MCATE_cfg` object.
        #' @examples
        #' MCATE_cfg$new(cfgs = list(x1 = KernelSmooth_cfg$new(neval = 100)))
        initialize = function(cfgs, std_errors=TRUE) {
            self$cfgs <- cfgs
            self$std_errors <- std_errors
            invisible(self)
        },
        add_moderator = function(var_name, cfg) {
            self$cfgs[[var_name]] <- cfg
            invisible(self)
        }
    )
)

#' Configuration of Partial CATEs
#'
#' @description
#' `PCATE_cfg` is a configuration class for estimating marginal response surfaces based on heterogeneous
#' treatment effect estimates. "Partial" in this context is used similarly to the use in partial dependence plots
#' or in partial regression. In essence, a PCATE attempts to partial out the contribution to the CATE from all other
#' covariates. Two highly correlated variables may have very different PCATE surfaces.
#' @export
PCATE_cfg <- R6::R6Class("PCATE_cfg",
    public = list(
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
        #' Create a new `PCATE_cfg` object with specified model name and hyperparameters.
        #' @param model_covariates A character vector of all the covariates to be included in the second-level
        #' effect regression.
        #' @param effect_cfg A `Model_cfg` object indicating how to fit the second level effect
        #' regression (joint across all selected covariates).
        #' @param cfgs Named list from moderator name to a `Model_cfg` object defining how to present
        #' that covariate's CATE surface.
        #' @param num_mc_samples A named list from covariate name to the number of Monte Carlo samples to take to
        #' calculate the double integral (See Details). If all covariates should use the same number of samples,
        #' simply pass the (integer) number of samples.
        #' @return A new `PCATE_cfg` object.
        #' @examples
        #' PCATE_cfg$new(
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
            invisible(self)
        },
        #' @description
        #' Add a moderator to the `PCATE_cfg` object. This entails adding it to the joint
        #' model of effects and defines a configuration for displaying the effect surface
        #' for that moderator.
        #' @param var_name The name of the moderator to add (and the name of the column in
        #' the dataset).
        #' @param A `Model_cfg` defining how to display the selected moderator's effect
        #' surface.
        add_moderator = function(var_name, cfg) {
            self$cfgs[[var_name]] <- cfg
            self$model_covariates <- unique(c(self$model_covariates, var_name))
            invisible(self)
        }
    )
)

#' Configuration of Variable Importance
#'
#' @description
#' `VIMP_cfg` is a configuration class for estimating a variable importance measure across all moderators.
#' This provides a meaningful measure of which moderators explain the most of the CATE surface.
#' @export
VIMP_cfg <- R6::R6Class("VIMP_cfg",
    public = list(
        #' @field model_cfg A `Model_cfg` object indicating how to fit the second level effect
        #' regression (joint across all moderators).
        model_cfg = list(),
        #' @field estimand String indicating the estimand to target.
        estimand = "VIMP",
        #' @field sample_splitting Logical indicating whether to use sample splitting in the calculation
        #' of variable importance.
        sample_splitting = TRUE,
        #' @description
        #' Create a new `VIMP_cfg` object with specified model configuration.
        #' @param model_cfg A `Model_cfg` object indicating how to fit the second level effect
        #' regression (joint across all moderators).
        #' @param sample_splitting Logical indicating whether to use sample splitting in the calculation
        #' of variable importance. Choosing not to use sample splitting means that inference will only be
        #' valid for moderators with non-null importance.
        #' @return A new `VIMP_cfg` object.
        #' @examples
        #' VIMP_cfg$new(model_cfg = SLLearner_cfg$new("SL.glm"))
        initialize = function(model_cfg, sample_splitting = TRUE) {
            soft_require("vimp")
            self$model_cfg <- model_cfg
            self$sample_splitting <- sample_splitting
            invisible(self)
        }
    )
)

#' Configuration of Model Diagnostics
#'
#' @description
#' `Diagnostics_cfg` is a configuration class for estimating a variety of diagnostics for the models
#' trained in the course of HTE estimation.
#' @export
Diagnostics_cfg <- R6::R6Class("Diagnostics_cfg",
    public = list(
        #' @field ps Model diagnostics for the propensity score model.
        ps = character(),
        #' @field outcome Model diagnostics for the outcome models.
        outcome = character(),
        #' @field effect Model diagnostics for the joint effect model.
        effect = character(),

        #' @description
        #' Create a new `Diagnostics_cfg` object with specified diagnostics to estimate.
        #' @param ps Model diagnostics for the propensity score model.
        #' @param outcome Model diagnostics for the outcome models.
        #' @param effect Model diagnostics for the joint effect model.
        #' @return A new `Diagnostics_cfg` object.
        #' @examples
        #' Diagnostics_cfg$new(
        #'    outcome = c("SL_risk", "SL_coefs", "MSE"),
        #'    ps = c("SL_risk", "SL_coefs", "AUC")
        #' )
        initialize = function(ps = NULL, outcome = NULL, effect = NULL) {
            if (!is.null(ps)) self$ps <- ps
            if (!is.null(outcome)) self$outcome <- outcome
            if (!is.null(effect)) self$effect <- effect
            invisible(self)
        },
        #' @description
        #' Add diagnostics to the `Diagnostics_cfg` object.
        #' @param ps Model diagnostics for the propensity score model.
        #' @param outcome Model diagnostics for the outcome models.
        #' @param effect Model diagnostics for the joint effect model.
        add = function(ps = NULL, outcome = NULL, effect = NULL) {
            if (!is.null(ps)) self$ps <- unique(tolower(c(self$ps, ps)))
            if (!is.null(outcome)) self$outcome <- unique(tolower(c(self$outcome, outcome)))
            if (!is.null(effect)) self$effect <- unique(tolower(c(self$effect, effect)))
            invisible(self)
        }
    )
)

#' Configuration of Quantities of Interest
#'
#' @description
#' `QoI_cfg` is a configuration class for the Quantities of Interest to be
#' generated by the HTE analysis.
#' @export
QoI_cfg <- R6::R6Class("QoI_cfg",
    public = list(
        #' @field mcate A configuration object of type `MCATE_cfg` of marginal effects to calculate.
        mcate = NULL,
        #' @field pcate A configuration object of type `PCATE_cfg` of partial effects to calculate.
        pcate = NULL,
        #' @field vimp A configuration object of type `VIMP_cfg` of variable importance to calculate.
        vimp = NULL,
        #' @field diag A configuration object of type `Diagnostics_cfg` of model diagnostics to calculate.
        diag = NULL,
        #' @field ate Logical flag indicating whether an estimate of the ATE should be returned.
        ate = logical(),

        #' @description
        #' Create a new `QoI_cfg` object with specified Quantities of Interest to estimate.
        #' @param mcate A configuration object of type `MCATE_cfg` of marginal effects to calculate.
        #' @param pcate A configuration object of type `PCATE_cfg` of partial effects to calculate.
        #' @param vimp A configuration object of type `VIMP_cfg` of variable importance to calculate.
        #' @param diag A configuration object of type `Diagnostics_cfg` of model diagnostics to calculate.
        #' @param ate A logical flag for whether to calculate the Average Treatment Effect (ATE) or not.
        #' @return A new `Diagnostics_cfg` object.
        #' @examples
        #' mcate_cfg <- MCATE_cfg$new(cfgs = list(x1 = KernelSmooth_cfg$new(neval = 100)))
        #' pcate_cfg <- PCATE_cfg$new(
        #'    cfgs = list(x1 = KernelSmooth_cfg$new(neval = 100)),
        #'    model_covariates = c("x1", "x2", "x3"),
        #'    effect_cfg = SLLearner_cfg$new("SL.glm"),
        #'    num_mc_samples = list(x1 = 100)
        #' )
        #' vimp_cfg <- VIMP_cfg$new(model_cfg = SLLearner_cfg$new("SL.glm"))
        #' diag_cfg <- Diagnostics_cfg$new(
        #'    outcome = c("SL_risk", "SL_coefs", "MSE"),
        #'    ps = c("SL_risk", "SL_coefs", "AUC")
        #' )
        #' QoI_cfg$new(
        #'     mcate = mcate_cfg,
        #'     pcate = pcate_cfg,
        #'     vimp = vimp_cfg,
        #'     diag = diag_cfg
        #' )
        initialize = function(
            mcate = NULL, pcate = NULL, vimp = NULL, diag = NULL, ate = TRUE
        ) {
            if (is.null(mcate) && is.null(pcate) && is.null(vimp) && is.null(diag)) {
                stop("Must define at least one QoI!")
            }
            if (!is.null(mcate)) self$mcate <- mcate
            if (!is.null(pcate)) self$pcate <- pcate
            if (!is.null(vimp)) self$vimp <- vimp
            if (!is.null(diag)) self$diag <- diag
            self$ate <- ate
            invisible(self)
        }
    )
)


#' Configuration of Quantities of Interest
#'
#' @description
#' `HTE_cfg` is a configuration class that pulls everything together, indicating
#' the full configuration for a given HTE analysis. This includes how to estimate
#' models and what Quantities of Interest to calculate based off those underlying models.
#' @export
HTE_cfg <- R6::R6Class("HTE_cfg",
    public = list(
        #' @field outcome `Model_cfg` object indicating how outcome models should be estimated.
        outcome = list(),
        #' @field treatment `Model_cfg` object indicating how the propensity score model should be estimated.
        treatment = list(),
        #' @field qoi `QoI_cfg` object indicating what the Quantities of Interest are and providing all
        #' necessary detail on how they should be estimated.
        qoi = list(),
        #' @field verbose Logical indicating whether to print debugging information.
        verbose = logical(),

        #' @description
        #' Create a new `HTE_cfg` object with all necessary information about how to carry out an HTE
        #' analysis.
        #' @param outcome `Model_cfg` object indicating how outcome models should be estimated.
        #' @param treatment `Model_cfg` object indicating how the propensity score model should be estimated.
        #' @param qoi `QoI_cfg` object indicating what the Quantities of Interest are and providing all
        #' necessary detail on how they should be estimated.
        #' @param verbose Logical indicating whether to print debugging information.
        #' @examples
        #' mcate_cfg <- MCATE_cfg$new(cfgs = list(x1 = KernelSmooth_cfg$new(neval = 100)))
        #' pcate_cfg <- PCATE_cfg$new(
        #'    cfgs = list(x1 = KernelSmooth_cfg$new(neval = 100)),
        #'    model_covariates = c("x1", "x2", "x3"),
        #'    effect_cfg = SLLearner_cfg$new("SL.glm"),
        #'    num_mc_samples = list(x1 = 100)
        #' )
        #' vimp_cfg <- VIMP_cfg$new(model_cfg = SLLearner_cfg$new("SL.glm"))
        #' diag_cfg <- Diagnostics_cfg$new(
        #'    outcome = c("SL_risk", "SL_coefs", "MSE"),
        #'    ps = c("SL_risk", "SL_coefs", "AUC")
        #' )
        #' qoi_cfg <- QoI_cfg$new(
        #'     mcate = mcate_cfg,
        #'     pcate = pcate_cfg,
        #'     vimp = vimp_cfg,
        #'     diag = diag_cfg
        #' )
        #' ps_cfg <- SLEnsemble_cfg$new(
        #'    learner_cfgs = list(SLLearner_cfg$new("SL.glm"), SLLearner_cfg$new("SL.gam"))
        #' )
        #' y_cfg <- SLEnsemble_cfg$new(
        #'    learner_cfgs = list(SLLearner_cfg$new("SL.glm"), SLLearner_cfg$new("SL.gam"))
        #' )
        #' HTE_cfg$new(outcome = y_cfg, treatment = ps_cfg, qoi = qoi_cfg)
        initialize = function(
            outcome = NULL, treatment = NULL, qoi = NULL, verbose = FALSE
        ) {
            if (is.null(outcome)) outcome <- SLEnsemble_cfg$new()
            if (is.null(treatment)) treatment <- SLEnsemble_cfg$new()
            if (is.null(qoi)) qoi <- QoI_cfg$new()
            self$outcome <- outcome
            self$treatment <- treatment
            self$qoi <- qoi
            self$verbose <- verbose
            invisible(self)
        }
    )
)
