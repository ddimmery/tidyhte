HTEFold <- R6::R6Class("HTEFold", list(
    train = NULL,
    holdout = NULL,
    in_holdout = NULL,
    initialize = function(.data, split_id) {
        if (!(".split_id" %in% names(.data))) {
            stop("Must construct split identifiers before splitting.")
        }
        num_splits <- length(unique(.data$.split_id))
        # check that the split_id is valid
        self$train <-  dplyr::filter(.data, .split_id != split_id)
        self$holdout <- dplyr::filter(.data, .split_id == split_id)
        self$in_holdout <- .data$.split_id == split_id
    }
))

#' Partition the data into folds
#'
#' This takes a dataset and a split ID and generates two subsets of the
#' data corresponding to a training set and a holdout.
#' @param .data dataframe
#' @param split_id integer representing the split to construct
#' @return Returns an R6 object `HTEFold` with three public fields:
#' - `train` - The split to be used for training the plugin estimates
#' - `holdout` - The split not used for training
#' - `in_holdout` - A logical vector indicating for each unit whether they lie in the holdout.
#' @export
split_data <- function(.data, split_id) {
    HTEFold$new(.data, split_id)
}


#' @importFrom stats model.matrix model.frame
Model_data <- R6::R6Class("Model_data", list(
    label = NULL,
    features = NULL,
    model_frame = NULL,
    split_id = NULL,
    num_splits = NULL,
    initialize = function(.data, label_col, ...) {
        self$label <- unlist(dplyr::select(.data, {{ label_col }}))
        self$features <- model.matrix(~. + 0, data = dplyr::select(.data, ...))
        self$model_frame <- model.frame(~. + 0, data = dplyr::select(.data, ...))
        if (".split_id" %in% names(.data)) {
            self$split_id <- .data$.split_id
            self$num_splits <- attr(.data, "num_splits")
        }
    },
    SL_cv_control = function() {
        validRows <- purrr::map(unique(self$split_id), ~which(.x == self$split_id))
        SuperLearner.CV.control(V = length(validRows), validRows = validRows)
    }
))
