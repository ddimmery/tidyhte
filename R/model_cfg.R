#' Configuration of SuperLearner Submodel
#'
#' @description
#' `SLLearner_cfg` is a configuration class for a single
#' sublearner to be included in SuperLearner. By constructing with a named list
#' of hyperparameters, this configuration allows distinct submodels
#' for each unique combination of hyperparameters. To understand what models
#' and hyperparameters are available, examine the methods listed in
#' `SuperLearner::listWrappers("SL")`.
#' @importFrom R6 R6Class
#' @export
SLLearner_cfg <- R6::R6Class("SLLearner_cfg",
    public = list(
        #' @field model_name The name of the model as passed to `SuperLearner`
        #' through the `SL.library` parameter.
        model_name = character(),
        #' @field hyperparameters Named list from hyperparameter name to a vector of
        #' values that should be swept over.
        hyperparameters = NULL,
        #' @description
        #' Create a new `SLLearner_cfg` object with specified model name and hyperparameters.
        #' @param model_name The name of the model as passed to `SuperLearner`
        #' through the `SL.library` parameter.
        #' @param hp Named list from hyperparameter name to a vector of values that should be
        #' swept over. Hyperparameters not included in this list are left at their SuperLearner
        #' default values.
        #' @return A new `SLLearner_cfg` object.
        #' @examples
        #' SLLearner_cfg$new("SL.glm")
        #' SLLearner_cfg$new("SL.gam", list(deg.gam = c(2, 3)))
        initialize = function(model_name, hp = NULL) {
            self$model_name <- model_name
            self$hyperparameters <- hp
            invisible(self)
        }
    )
)

#' Base Class of Model Configurations
#'
#' @description
#' `Model_cfg` is the base class from which all other model configurations
#' inherit.
#' @importFrom R6 R6Class
#' @export
Model_cfg <- R6::R6Class("Model_cfg",
    list(
        #' @field model_class The class of the model, required for all classes
        #' which inherit from `Model_cfg`.
        model_class = character(length = 1),

        #' @description
        #' Create a new `Model_cfg` object with any necessary parameters.
        #' @return A new `Model_cfg` object.
        initialize = function() {
            invisible(self)
        }
    )
)

#' Configuration of Known Model
#'
#' @description
#' `Known_cfg` is a configuration class for when a particular model is known
#' a-priori. The prototypical usage of this class is when heterogeneous
#' treatment effects are estimated in the context of a randomized control
#' trial with known propensity scores.
#' @importFrom R6 R6Class
#' @export
Known_cfg <- R6::R6Class("Known_cfg",
    inherit = Model_cfg,
    public = list(
        #' @field covariate_name The name of the column in the dataset
        #' which corresponds to the known model score.
        covariate_name = character(),
        #' @field model_class The class of the model, required for all classes
        #' which inherit from `Model_cfg`.
        model_class = "known",

        #' @description
        #' Create a new `Known_cfg` object with specified covariate column.
        #' @param covariate_name The name of the column, a string, in the dataset
        #' corresponding to the known model score (i.e. the true conditional expectation).
        #' @return A new `Known_cfg` object.
        #' @examples
        #' Known_cfg$new("propensity_score")
        initialize = function(covariate_name) {
            self$covariate_name <- covariate_name
            invisible(self)
        }
    )
)

#' Configuration of a Constant Estimator
#'
#' @description
#' `Constant_cfg` is a configuration class for estimating a constant model.
#' That is, the model is a simple, one-parameter mean model.
#' @importFrom R6 R6Class
#' @export
Constant_cfg <- R6::R6Class("Constant_cfg",
    inherit = Model_cfg,
    public = list(
        #' @field model_class The class of the model, required for all classes
        #' which inherit from `Model_cfg`.
        model_class = "Constant",

        #' @description
        #' Create a new `Constant_cfg` object.
        #' @return A new `Constant_cfg` object.
        #' @examples
        #' Constant_cfg$new()
        initialize = function() {
            invisible(self)
        }
    )
)


#' Configuration for a Kernel Smoother
#'
#' @description
#' `KernelSmooth_cfg` is a configuration class for non-parametric local-linear
#' regression to construct a smooth representation of the relationship between
#' two variables. This is typically used for displaying a surface of the conditional
#' average treatment effect over a continuous covariate.
#'
#' Kernel smoothing is handled by the `nprobust` package.
#' @seealso [nprobust::lprobust]
#' @importFrom R6 R6Class
#' @export
KernelSmooth_cfg <- R6::R6Class("KernelSmooth_cfg",
    inherit = Model_cfg,
    public = list(
        #' @field model_class The class of the model, required for all classes
        #' which inherit from `Model_cfg`.
        model_class = "KernelSmooth",
        #' @field neval The number of points at which to evaluate the local
        #' regression. More points will provide a smoother line at the cost
        #' of somewhat higher computation.
        neval = integer(),
        #' @field eval_min_quantile Minimum quantile at which to evaluate the smoother.
        eval_min_quantile = double(),

        #' @description
        #' Create a new `KernelSmooth_cfg` object with specified number of evaluation points.
        #' @param neval The number of points at which to evaluate the local
        #' regression. More points will provide a smoother line at the cost
        #' of somewhat higher computation.
        #' @param eval_min_quantile Minimum quantile at which to evaluate the smoother.
        #' A value of zero will do no clipping. Clipping is performed from both the
        #' top and the bottom of the empirical distribution. A value of alpha would
        #' evaluate over \[alpha, 1 - alpha\].
        #' @return A new `KernelSmooth_cfg` object.
        #' @examples
        #' KernelSmooth_cfg$new(neval = 100)
        initialize = function(neval = 100, eval_min_quantile = 0.05) {
            soft_require("nprobust")
            self$neval <- neval
            eval_min_quantile <- pmin(eval_min_quantile, 1 - eval_min_quantile)
            checkmate::check_double(eval_min_quantile, lower = 0.0, upper = 0.5)
            self$eval_min_quantile <- eval_min_quantile
            invisible(self)
        }
    )
)


#' Configuration for a Stratification Estimator
#'
#' @description
#' `Stratified_cfg` is a configuration class for stratifying a covariate
#' and calculating statistics within each cell.
#' @importFrom R6 R6Class
#' @export
Stratified_cfg <- R6::R6Class("Stratified_cfg",
    inherit = Model_cfg,
    public = list(
        #' @field model_class The class of the model, required for all classes
        #' which inherit from `Model_cfg`.
        model_class = "Stratified",
        #' @field covariate The name of the column in the dataset
        #' which corresponds to the covariate on which to stratify.
        covariate = character(),

        #' @description
        #' Create a new `Stratified_cfg` object with specified number of evaluation points.
        #' @param covariate The name of the column in the dataset
        #' which corresponds to the covariate on which to stratify.
        #' @return A new `Stratified_cfg` object.
        #' @examples
        #' Stratified_cfg$new(covariate = "test_covariate")
        initialize = function(covariate) {
            self$covariate <- covariate
            invisible(self)
        }
    )
)

#' Configuration for a SuperLearner Ensemble
#'
#' @description
#' `SLEnsemble_cfg` is a configuration class for estimation of a model
#' using an ensemble of models using `SuperLearner`.
#' @import SuperLearner
#' @importFrom R6 R6Class
#' @examples
#' SLEnsemble_cfg$new(
#' learner_cfgs = list(SLLearner_cfg$new("SL.glm"), SLLearner_cfg$new("SL.gam"))
#' )
#' @export
SLEnsemble_cfg <- R6::R6Class("SLEnsemble_cfg",
    inherit = Model_cfg,
    public = list(
        #' @field cvControl A list of parameters for controlling the
        #' cross-validation used in SuperLearner.
        cvControl = list(V = 10),
        #' @field SL.library A vector of the names of learners to
        #' include in the SuperLearner ensemble.
        SL.library = character(),
        #' @field SL.env An environment containing all of the programmatically
        #' generated learners to be included
        #' in the SuperLearner ensemble.
        SL.env = NULL,
        #' @field family `stats::family` object to determine how SuperLearner
        #' should be fitted.
        family = list(),
        #' @field model_class The class of the model, required for all classes
        #' which inherit from `Model_cfg`.
        model_class = "SL",

        #' @description
        #' Create a new `SLEnsemble_cfg` object with specified settings.
        #' @param cvControl A list of parameters for controlling the
        #' cross-validation used in SuperLearner.
        #' For more details, see `SuperLearner::SuperLearner.CV.control`.
        #' @param learner_cfgs A list of `SLLearner_cfg` objects.
        #' @param family `stats::family` object to determine how SuperLearner should be fitted.
        #' @return A new `SLEnsemble_cfg` object.
        #' @examples
        #' SLEnsemble_cfg$new(
        #' learner_cfgs = list(SLLearner_cfg$new("SL.glm"), SLLearner_cfg$new("SL.gam"))
        #' )
        initialize = function(cvControl = NULL, learner_cfgs = NULL, family = stats::gaussian()) {
            soft_require("SuperLearner", load = TRUE)

            self$family <- family
            if (!is.null(cvControl)) {
                self$cvControl <- cvControl
            }
            if (is.null(learner_cfgs)) {
                learner_cfgs <- list(SLLearner_cfg$new("SL.glm"))
            }
            self$SL.env <- new.env()
            self$SL.library <- character()
            sl_ns <- as.list(getNamespace("SuperLearner"), all.names = TRUE)
            for (name in names(sl_ns)) {
                assign(name, sl_ns[[name]], envir = self$SL.env)
            }
            for (lrnr in learner_cfgs) {
                self$add_sublearner(lrnr$model_name, lrnr$hyperparameters)
            }
            invisible(self)
        },
        #' @description
        #' Adds a model (or class of models) to the SuperLearner ensemble.
        #' If hyperparameter values are specified, this method will
        #' add a learner for every element in the cross-product of provided
        #' hyperparameter values.
        #' @param learner_name Possible values
        #' use `SuperLearner` naming conventions. A full list is available
        #' with `SuperLearner::listWrappers("SL")`
        #' @param hps A named list of hyper-parameters. Every element of the
        #' cross-product of these hyper-parameters will be included in the
        #' ensemble.
        #' cfg <- SLEnsemble_cfg$new(
        #'  learner_cfgs = list(SLLearner_cfg$new("SL.glm"))
        #' )
        #' cfg <- cfg$add_sublearner("SL.gam", list(deg.gam = c(2, 3)))
        add_sublearner = function(learner_name, hps = NULL) {
            sl_lib <- character()
            if (is.null(hps)) {
                lrnrs <- learner_name
            } else {
                learners <- create.Learner(
                    learner_name,
                    tune = hps,
                    detailed_names = TRUE,
                    name_prefix = paste0("custom_", learner_name),
                    env = self$SL.env
                )
                lrnrs <- learners$names
            }
            sl_lib <- c(sl_lib, lrnrs)
            self$SL.library <- unique(c(self$SL.library, sl_lib))
            invisible(self)
        }
    )
)
