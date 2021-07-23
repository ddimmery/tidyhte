
#' Define splits for cross-fitting
#'
#' This takes a dataset, a column with a unique identifier and an
#' arbitrary number of covariates on which to stratify the splits.
#' It returns the original dataset with an additional column `.split_id`
#' corresponding to an identifier for the split.
#' @param .data dataframe
#' @param id_col unquoted name of unique identifier column
#' @param ... variables on which to stratify
#' @param .num_splits number of splits to create
#' @return dataframe with additional `.split_id` column
#' @export
make.splits <- function(.data, id_col, ..., .num_splits) {
    .data %>%
    dplyr::group_by({{id_col}}, ...) %>%
    dplyr::tally() %>%
    dplyr::group_by(...) %>%
    dplyr::mutate(
        .split_id = sample(c(
            rep(
                1:.num_splits,
                rep(as.integer(floor(dplyr::n() / .num_splits), .num_splits))
            ),
            sample(
                .num_splits,
                (
                    dplyr::n() -
                    .num_splits * as.integer(floor(dplyr::n() / .num_splits))
                )
            )
        ))
    ) %>%
    dplyr::select(-n) -> tmp
    join_cols <- names(dplyr::select(tmp, -.split_id))
    dplyr::left_join(.data, tmp, by = join_cols)
}

#' @export
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
        self$train <-  dplyr::filter(.data, .split_id == split_id)
        self$holdout <- dplyr::filter(.data, .split_id != split_id)
        self$in_holdout <- .data$.split_id != split_id
    }
))

#' Given a particular split ID, partition the data
#'
#' This takes a dataset and a split ID and generates two subsets of the
#' data corresponding to a training set and a holdout.
#' @param .data dataframe
#' @param split_id integer representing the split to construct
#' @return Returns an R6 object `HTEFold` with two properties:
#' - `train` - The split to be used for training the plugin estimates
#' - `holdout` - The split not used for training
#' @export
split_data <- function(.data, split_id) {
    HTEFold$new(.data, split_id)
}


#' @export
Model.data <- R6::R6Class("Model.data", list(
    label = NULL,
    features = NULL,
    model_frame = NULL,
    initialize = function(.data, label_col, ...) {
        self$label <- unlist(dplyr::select(.data, {{ label_col }}))
        self$features <- model.matrix(~. + 0, data = dplyr::select(.data, ...))
        self$model_frame <- model.frame(~. + 0, data = dplyr::select(.data, ...))
    }
))
