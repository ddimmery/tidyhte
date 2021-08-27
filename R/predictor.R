predictor_factory <- function(cfg, ...) {
    if (cfg$model_class == "known") {
        KnownPredictor$new(cfg$covariate_name)
    } else if (cfg$model_class == "SL") {
        SLPredictor$new(cfg$SL.library, cfg$SL.env, family = cfg$family, ...)
    } else if (cfg$model_class == "KernelSmooth") {
        KernelSmoothPredictor$new(neval = cfg$neval)
    } else if (cfg$model_class == "Stratified") {
        StratifiedPredictor$new(cfg$covariate)
    } else{
        stop("Unknown model class.")
    }
}


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


KnownPredictor <- R6::R6Class("KnownPredictor",
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
SLPredictor <- R6::R6Class("SLPredictor",
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
                SL.library = self$SL.library, env = self$SL.env, cvControl = data$SL_cv_control()
            )
            invisible(self)
        },
        predict = function(data) {
            muffle_warnings(
                drop(predict(self$model, newdata = data$model_frame)$pred),
                "rank-deficient fit"
            )
        }
    )
)

#' @export
#' @importFrom nprobust lprobust
KernelSmoothPredictor <- R6::R6Class("KernelSmoothPredictor",
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
            if (length(unique(self$covariates)) == length(self$covariates)) {
                bw <- "imse-dpi"
                cluster <- NULL
            } else {
                bw <- "imse-rot"
                cluster <- as.factor(as.integer(as.factor(self$covariates)))
            }
            self$model <- nprobust::lprobust(
                self$label,
                self$covariates,
                neval = self$neval,
                bwselect = bw,
                # cluster = cluster,
                # vce = "hc0"
            )
            invisible(self)
        },
        predict = function(data) {
            ests <- self$model$Estimate[, c("eval", "tau.bc", "N")]
            dplyr::tibble(
                x = ests[, 1],
                estimate = ests[, 2],
                sample_size = ests[, 3]
            )
        },
        predict_se = function(data) {
            ests <- self$model$Estimate[, c("eval", "tau.bc", "se.rb", "N")]
            dplyr::tibble(
                x = ests[, 1],
                estimate = ests[, 2],
                std_error = ests[, 3],
                sample_size = ests[, 4]
            )
        }
    )
)

#' @export
#' @importFrom stats sd
#' @importFrom rlang .data
#' @importFrom magrittr %>%
StratifiedPredictor <- R6::R6Class("StratifiedPredictor",
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
                dplyr::group_by(.data$x) %>%
                dplyr::summarize(
                    estimate = mean(.data$y),
                    std_error = stats::sd(.data$y) / sqrt(dplyr::n()),
                    sample_size = dplyr::n()
                )
            invisible(self)
        },
        predict = function(data) {
            # return the means for the requested buckets
            # check the overlap between requested buckets and available ones
            unq_vals <- unique(unlist(data$model_frame))
            dplyr::tibble(x = unq_vals, idx = seq_len(length(unq_vals))) %>%
                dplyr::inner_join(self$map, by = "x") %>%
                dplyr::arrange(.data$idx) %>%
                dplyr::select(.data$x, .data$estimate, .data$sample_size)
        },
        predict_se = function(data) {
            # return mean and std err for requested buckets
            unq_vals <- unique(unlist(data$model_frame))
            dplyr::tibble(x = unq_vals, idx = seq_len(length(unq_vals))) %>%
                dplyr::inner_join(self$map, by = "x") %>%
                dplyr::arrange(.data$idx) %>%
                dplyr::select(.data$x, .data$estimate, .data$std_error, .data$sample_size)
        }
    )
)
