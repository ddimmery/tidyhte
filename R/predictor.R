predictor_factory <- function(cfg, ...) {
    if (!("model_class" %in% names(cfg))) {
        stop("Unknown model class.")
    } else if (cfg$model_class == "known") {
        KnownPredictor$new(cfg$covariate_name)
    } else if (cfg$model_class == "SL") {
        SLPredictor$new(cfg$SL.library, cfg$SL.env, family = cfg$family, ...)
    } else if (cfg$model_class == "KernelSmooth") {
        KernelSmoothPredictor$new(neval = cfg$neval, eval_min_quantile = cfg$eval_min_quantile)
    } else if (cfg$model_class == "Stratified") {
        StratifiedPredictor$new(cfg$covariate)
    } else if (cfg$model_class == "Constant") {
        ConstantPredictor$new()
    } else {
        stop("Unknown model class.")
    }
}


Predictor <- R6::R6Class("Predictor",
    list(
        initialize = function(...) {
            warning("Not Implemented")
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
            dplyr::tibble(
                x = rep(NA_real_, nrow(data$model_frame)),
                estimate = data$model_frame[[self$covariate]],
                sample_size = rep(NA_real_, nrow(data$model_frame))
            )
        }
    )
)


ConstantPredictor <- R6::R6Class("ConstantPredictor",
    inherit = Predictor,
    public = list(
        model = NULL,
        covariate = character(),
        initialize = function() {
        },
        fit = function(data) {
            self$model <- dplyr::tibble(
                x = NA,
                estimate = mean(data$label),
                sample_size = length(unique(data$cluster))
            )
            invisible(self)
        },
        predict = function(data) {
            self$model
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
            # needs to be rewritten to pass out a df w/ x, estimate, sample_size
            pred <- muffle_warnings(
                drop(predict(self$model, newdata = data$model_frame)$pred),
                "rank-deficient fit"
            )
            covs <- rep(NA_real_, length(pred))
            if (ncol(data$model_frame) == 1) covs <- drop(unlist(data$model_frame))
            dplyr::tibble(
                x = covs,
                estimate = pred,
                sample_size = rep(1, length(pred))
            )
        }
    )
)


KernelSmoothPredictor <- R6::R6Class("KernelSmoothPredictor",
    inherit = Predictor,
    list(
        model = NULL,
        label = character(),
        covariates = character(),
        neval = integer(),
        eval_min_quantile = double(),
        initialize = function(neval, eval_min_quantile) {
            self$neval <- neval
            self$eval_min_quantile <- eval_min_quantile
        },
        fit = function(data) {
            self$label <- data$label
            self$covariates <- drop(data$features)
            eval_pts <- quantile(
                self$covariates,
                probs = seq(self$eval_min_quantile, 1 - self$eval_min_quantile, length = self$neval)
            )
            if (length(unique(self$covariates)) == length(self$covariates)) {
                self$model <- nprobust::lprobust(
                    self$label,
                    self$covariates,
                    eval = eval_pts
                )
            } else {
                agg_tbl <- dplyr::tibble(y = self$label, x = self$covariates) %>%
                    dplyr::group_by(.data$x) %>%
                    dplyr::summarize(y = mean(.data$y), n = dplyr::n())
                agg_y <- agg_tbl$y
                agg_x <- agg_tbl$x
                bws <- nprobust::lpbwselect(agg_y, agg_x, eval = eval_pts, bwselect = "imse-dpi")
                self$model <- nprobust::lprobust(
                    self$label,
                    self$covariates,
                    eval = eval_pts,
                    #cluster = data$cluster,
                    #cluster = factor(self$covariates), # data$cluster
                    h = bws$bws[, "h"]
                )
            }
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
            self$map <- dplyr::tibble(x = unlist(data$model_frame), y = data$label, cluster = data$cluster) %>%
            dplyr::group_by(.data$x) %>%
            dplyr::summarize(
                estimate = mean(.data$y),
                std_error = clustered_se_of_mean(.data$y, .data$cluster),
                sample_size = length(unique(.data$cluster))
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
