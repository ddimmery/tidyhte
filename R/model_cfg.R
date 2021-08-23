
#' @export
SLLearner_cfg <- R6::R6Class("SLLearner_cfg",
    list(
        model_name = character(),
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
        model_class = "SL",
        initialize = function(cvControl = NULL, learner_cfgs = NULL) {
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
