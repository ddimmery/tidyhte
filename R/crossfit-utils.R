
#' Define splits for cross-fitting
#' 
#' This takes a dataset, a column with a unique identifier and an
#' arbitrary number of covariates on which to stratify the splits.
#' It returns the original dataset with an additional column `split_id`
#' corresponding to an identifier for the split.
#' @param .data dataframe
#' @param id_col unquoted name of unique identifier column
#' @param ... variables on which to stratify
#' @param .num_splits number of splits to create
#' @return dataframe with additional `split_id` column
#' @export
make.splits <- function(.data, id_col, ..., .num_splits) {
    .data %>%
    dplyr::group_by({{id_col}}, ...) %>%
    dplyr::tally() %>%
    dplyr::group_by(...) %>%
    dplyr::mutate(
        split_id=sample(c(
            rep(1:.num_splits, rep(as.integer(floor(dplyr::n() / .num_splits), .num_splits))),
            sample(.num_splits, dplyr::n() - .num_splits * as.integer(floor(dplyr::n() / .num_splits)))
        ))
    ) %>%
    dplyr::select(-n) -> tmp
    join_cols = tmp %>% dplyr::select(-split_id) %>% names
    dplyr::left_join(.data, tmp, by=join_cols)
}

#' @export
HTEFold = R6::R6Class("HTEFold", list(
    train_regression = NULL,
    train_effects = NULL,
    holdout = NULL,
    initialize = function(.data, split_id) {
        if(!('split_id' %in% names(.data))) stop("Must construct split identifiers before splitting.")
        num_splits = length(unique(.data$split_id))
        effect_split = split_id
        holdout_split = (split_id + 1) %% num_splits
        self$train_regression = .data %>% dplyr::filter(split_id != effect_split, split_id != holdout_split)
        self$train_effects = .data %>% dplyr::filter(split_id == effect_split)
        self$holdout = .data %>% dplyr::filter(split_id == holdout_split)
    }
))

#' Given a particular split ID, partition the data
#' 
#' This takes a dataset and a split ID and generates three subsets of the 
#' data corresponding to two training sets and a holdout.
#' @param .data dataframe
#' @param split_id integer representing the split to construct
#' @return Returns an R6 object `HTEFold` with three properties:
#' - `train_regression` - The split to be used for training the plugin estimates
#' - `train_effects` - The split to be used for fitting effect models
#' - `holdout` - The split not used for training
#' @export
split.data <- function(.data, split_id) {
    HTEFold$new(.data, split_id)
}

