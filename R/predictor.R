#' @noRd
#' @keywords internal
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

#' @seealso [KnownPredictor], [ConstantPredictor], [SLPredictor], [KernelSmoothPredictor],
#' [StratifiedPredictor], [Model_cfg]
#' @keywords internal
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

#' @noRd
#' @keywords internal
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

#' @noRd
#' @keywords internal
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
                estimate = weighted.mean(data$label, data$weights),
                sample_size = length(unique(data$cluster))
            )
            invisible(self)
        },
        predict = function(data) {
            self$model
        }
    )
)

#' @noRd
#' @import SuperLearner
#' @keywords internal
SLPredictor <- R6::R6Class("SLPredictor",
    inherit = Predictor,
    public = list(
        model = list(),
        model_features = character(),
        SL.library = character(),
        SL.env = NULL,
        family = list(),
        initialize = function(SL.library, SL.env, family = stats::gaussian()) {
            self$SL.library <- SL.library
            self$SL.env <- SL.env
            self$family <- family
        },
        fit = function(data) {
            X_df <- tibble::as_tibble(data$features)
            self$model_features <- names(X_df)
            self$model <- muffle_warnings(SuperLearner::SuperLearner(
                Y = data$label, X = X_df, family = self$family,
                SL.library = self$SL.library, env = self$SL.env, cvControl = data$SL_cv_control(),
                obsWeights = data$weights
            ), "rank-deficient fit")
            invisible(self)
        },
        predict = function(data) {
            X_df <- tibble::as_tibble(data$features)
            missing_features <- setdiff(self$model_features, names(X_df))
            for (missing_feature in missing_features) {
                X_df[[missing_feature]] <- 0
            }
            pred <- muffle_warnings(
                drop(predict(self$model, newdata = X_df)$pred),
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

#' @noRd
#' @keywords internal
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
            if (any(data$weights != 1)) stop("`nprobust` does not support the use of weights.")
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
                    dplyr::group_by(x) %>%
                    dplyr::summarize(y = mean(y), n = dplyr::n())
                agg_y <- agg_tbl$y
                agg_x <- agg_tbl$x
                bws <- nprobust::lpbwselect(agg_y, agg_x, eval = eval_pts, bwselect = "imse-dpi")
                self$model <- nprobust::lprobust(
                    self$label,
                    self$covariates,
                    eval = eval_pts,
                    # cluster = data$cluster, # no-lint
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

#' @noRd
#' @importFrom stats sd
#' @importFrom magrittr %>%
#' @keywords internal
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
            self$map <- dplyr::tibble(
                x = unlist(data$model_frame),
                y = data$label,
                cluster = data$cluster,
                weights = data$weights
                ) %>%
            dplyr::group_by(x) %>%
            dplyr::summarize(
                estimate = weighted.mean(y),
                std_error = clustered_se_of_mean(y, cluster, weights = weights),
                sample_size = length(unique(cluster))
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
                dplyr::select(x, estimate, sample_size)
        },
        predict_se = function(data) {
            # return mean and std err for requested buckets
            unq_vals <- unique(unlist(data$model_frame))
            dplyr::tibble(x = unq_vals, idx = seq_len(length(unq_vals))) %>%
                dplyr::inner_join(self$map, by = "x") %>%
                dplyr::arrange(idx) %>%
                dplyr::select(x, estimate, std_error, sample_size)
        }
    )
)
