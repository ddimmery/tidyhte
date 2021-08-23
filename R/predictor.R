#' @export
predictor_factory <- function(cfg, ...) {
    if (cfg$model_class == "known") {
        Predictor.known$new(cfg$covariate_name)
    } else if (cfg$model_class == "SL") {
        Predictor.SL$new(cfg$SL.library, cfg$SL.env, ...)
    } else if (cfg$model_class == "KernelSmooth") {
        Predictor.KernelSmooth$new(neval = cfg$neval)
    } else if (cfg$model_class == "Stratified") {
        Predictor.Stratified$new(cfg$covariate)
    } else{
        stop("Unknown model class.")
    }
}

#' @export
Predictor <- R6::R6Class("Predictor",
    list(
        initialize = function(...) {
            stop("Not Implemented")
        },
        fit = function(labels, features) {
            stop("Not Implemented")
        },
        predict = function(features) {
            stop("Not Implemented")
        },
        predict_se = function(features) {
            stop("Not Implemented")
        }
    )
)

#' @export
Predictor.known <- R6::R6Class("Predictor.known",
    inherit = Predictor,
    public = list(
        model = NULL,
        covariate = character(),
        initialize = function(covariate) {
            self$covariate <- covariate
        },
        fit = function(data) {
            invisible(self)
        },
        predict = function(data) {
            data$model_frame[[self$covariate]]
        }
    )
)

#' @import SuperLearner
#' @export
Predictor.SL <- R6::R6Class("Predictor.SL",
    inherit = Predictor,
    public = list(
        model = list(),
        SL.library = character(),
        SL.env = NULL,
        family = list(),
        initialize = function(SL.library, SL.env, family=stats::gaussian()) {
            self$SL.library <- SL.library
            self$SL.env <- SL.env
            self$family <- family
        },
        fit = function(data) {
            self$model <- SuperLearner::SuperLearner(
                Y = data$label, X = data$model_frame, family = self$family,
                SL.library = self$SL.library, env = self$SL.env
            )
            invisible(self)
        },
        predict = function(data) {
            drop(predict(self$model, newdata = data$model_frame)$pred)
        }
    )
)

#' @export
Predictor.KernelSmooth <- R6::R6Class("Predictor.KernelSmooth",
    inherit = Predictor,
    list(
        model = NULL,
        label = character(),
        covariates = character(),
        neval = integer(),
        initialize = function(neval) {
            self$neval <- neval
        },
        fit = function(data) {
            # check that covariate is just 1D
            self$label <- data$label
            self$covariates <- drop(data$features)
            # terrible hack in case there are empty bandwidths
            # if (length(unique(self$covariates)) != length(self$covariates))
            #     self$covariates <- self$covariates + rnorm(length(self$covariates), 0, sd(self$covariates) / 1e6)
            if (length(unique(self$covariates)) == length(self$covariates)) {
                bw <- 'imse-dpi'
                cluster <- NULL
            } else {
                bw <- 'imse-rot'
                cluster <- as.integer(as.factor(self$covariates))
            }
            self$model <- nprobust::lprobust(self$label, self$covariates, neval = self$neval, bwselect = bw)
            invisible(self)
        },
        predict = function(data) {
            self$model$Estimate[, "tau.bc"]
        },
        predict_se = function(data) {
            ests <- self$model$Estimate[, c("eval", "tau.bc", "se.rb")]
            dplyr::tibble(
                x = ests[, 1],
                estimate = ests[, 2],
                std.error = ests[, 3]
            )
        }
    )
)

#' @export
Predictor.Stratified <- R6::R6Class("Predictor.Stratified",
    inherit = Predictor,
    list(
        model = NULL,
        covariate = character(),
        map = NULL,
        initialize = function(covariate) {
            self$covariate <- covariate
        },
        fit = function(data) {
            #check number of levels of covariate - should be less than 50 or so
            # throw warning if there's very sparse strata
            # create mapping from unique covariate values to mean + std err
            self$map <- dplyr::tibble(x = unlist(data$model_frame), y = data$label) %>%
                dplyr::group_by(x) %>%
                dplyr::summarize(
                    estimate = mean(y),
                    std.error = sd(y) / sqrt(dplyr::n())
                )
            invisible(self)
        },
        predict = function(data) {
            # return the means for the requested buckets
            # check the overlap between requested buckets and available ones
            unq_vals <- unique(unlist(data$model_frame))
            dplyr::tibble(x = unq_vals, idx = seq_len(length(unq_vals))) %>%
                dplyr::inner_join(self$map, by = "x") %>%
                dplyr::arrange(idx) %>%
                dplyr::select(estimate) %>%
                unlist()
        },
        predict_se = function(data) {
            # return mean and std err for requested buckets
            unq_vals <- unique(unlist(data$model_frame))
            dplyr::tibble(x = unq_vals, idx = seq_len(length(unq_vals))) %>%
                dplyr::inner_join(self$map, by = "x") %>%
                dplyr::arrange(idx) %>%
                dplyr::select(x, estimate, std.error)
        }
    )
)