#' 
#' @export
crossfit.hte <- function(.data, id_col, y_col, a_col, ..., .num_splits, .hte_config=NULL) {
    if(is.null(.hte_config)) .hte_config = HTE_cfg$new()
    .data = make.splits(d, id_col, .num_splits=.num_splits)
    hte = rep(NA_real_, nrow(.data))
    for(split_id in 1:(num_splits - 1)) {
        folds = split.data(.data, split_id)
        a_model = fit.plugin.A(folds$train_regression, a_col, ..., .SL.cfg=.hte_config$treatment)
        y_model = fit.plugin.Y(folds$train_regression, y_col, a_col, ..., .SL.cfg=.hte_config$outcome)
        eff = y_model$predict(folds$train_effects)
        ho = y_model$predict(folds$holdout)
    }
    
}

Learner.cfg = R6::R6Class("Learner.cfg",
    list(
        model_name=character(),
        hyperparameters=NULL,
        initialize=function(model_name, hp=NULL) {
            self$model_name = model_name
            self$hyperparameters = hp
        }
    )
)

Model.cfg = R6::R6Class("Model.cfg",
    list(
        cvControl=list(V=10),
        SL.library=character(),
        initialize=function(cvControl=NULL, learner_cfgs=NULL) {
            if(!is.null(cvControl)) self$cvControl=cvControl
            if(is.null(learner_cfgs)) learner_cfgs = Learner.cfg$new('SL.glm')
            SL.library = character()
            for(lrnr in learner_cfgs) {
                learner_name = lrnr$model_name 
                hyperparameters = lrnr$hyperparameters
                if(learner_name == 'known') self$covariate_name = lrnr$hyperparameters$covariate_name
                if(is.null(hyperparameters)) {
                    lrnrs = learner_name
                } else {
                    learners = create.Learner(learner_name, tune=hyperparameters, detailed_names = TRUE, name_prefix = paste0('custom_', learner_name))
                    lrnrs = learners$names
                }
                sl_lib = c(sl_lib, lrnrs)
            }
            self$SL.library = unique(sl_lib)
        }
    )
)

#' @export
HTE_cfg = R6::R6Class("HTE_cfg",
    list(
        outcome=list(),
        treatment=list(),
        effect=list(),
        initialize=function(outcome=NULL, treatment=NULL, effect=NULL) {
            if(is.null(outcome)) outcome = Model.cfg$new()
            if(is.null(treatment)) treatment = Model.cfg$new()
            if(is.null(effect)) effect = Model.cfg$new()
            self$outcome = outcome
            self$treatment = treatment
            self$effect = effect
        }
    )
)