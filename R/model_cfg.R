#' Configuration of SuperLearner Submodel
#'
#' @description
#' `SLLearner_cfg` is a configuration class for a single
#' sublearner to be included in SuperLearner. By constructing with a named list
#' of hyperparameters, this configuration allows distinct submodels
#' for each unique combination of hyperparameters.
#' @export
SLLearner_cfg <- R6::R6Class("SLLearner_cfg",
    public = list(
        #' @field model_name The name of the model as passed to `SuperLearner`
        #' through the `SL.library` parameter.
        model_name = character(),

        #' @field hyperparameters Named list from hyperparameter name to a vector of
        #' values that should be swept over.

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
        hyperparameters = NULL,
        initialize = function(model_name, hp=NULL) {
            self$model_name <- model_name
            self$hyperparameters <- hp
        }
    )
)

Model_cfg <- R6::R6Class("Model_cfg",
    list(
        model_class = NULL,
        initialize = function() {
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
#' @export
KernelSmooth_cfg <- R6::R6Class("KernelSmooth_cfg",
    inherit = Model_cfg,
    public = list(
        #' @field covariate_name The name of the column in the dataset
        #' which corresponds to the known model score.
        model_class = "KernelSmooth",
        #' @field neval The number of points at which to evaluate the local
        #' regression. More points will provide a smoother line at the cost
        #' of somewhat higher computation.
        neval = integer(),

        #' @description
        #' Create a new `KernelSmooth_cfg` object with specified number of evaluation points.
        #' @param neval The number of points at which to evaluate the local
        #' regression. More points will provide a smoother line at the cost
        #' of somewhat higher computation.
        #' @return A new `KernelSmooth_cfg` object.
        #' @examples
        #' KernelSmooth_cfg$new(neval = 100)
        initialize = function(neval = 100) {
            self$neval <- neval
        }
    )
)


#' @export
Stratified_cfg <- R6::R6Class("Stratified_cfg",
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
SLEnsemble_cfg <- R6::R6Class("SLEnsemble_cfg",
    inherit = Model_cfg,
    public = list(
        cvControl = list(V = 10),
        SL.library = character(),
        SL.env = NULL,
        family = list(),
        model_class = "SL",
        initialize = function(cvControl = NULL, learner_cfgs = NULL, family = stats::gaussian()) {
            self$family <- family
            if (!is.null(cvControl)) {
                self$cvControl <- cvControl
            }
            if (is.null(learner_cfgs)) {
                learner_cfgs <- list(SLLearner_cfg$new("SL.glm"))
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
