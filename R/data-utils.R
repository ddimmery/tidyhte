#' R6 class to represent partitions of the data between training and held-out
#'
#' This takes a set of folds calculated elsewhere and represents
#' these folds in a consistent format.
#' @keywords internal
HTEFold <- R6::R6Class("HTEFold", list(
    #' @field train A dataframe containing only the training set
    train = NULL,
    #' @field holdout A dataframe containing only the held-out data
    holdout = NULL,
    #' @field in_holdout A logical vector indicating if the initial data
    #' lies in the holdout set.
    in_holdout = NULL,
    #' @description
    #' Creates an R6 object of the data split between training and test set.
    #' @param data The dataset to be split
    #' @param split_id An identifier indicating which data should lie in the holdout set.
    #' @return Returns an object of class `HTEFold`
    initialize = function(data, split_id) {
        if (!(".split_id" %in% names(data))) {
            stop("Must construct split identifiers before splitting.")
        }
        num_splits <- length(unique(data$.split_id))
        # check that the split_id is valid
        self$train <-  dplyr::filter(data, .split_id != split_id)
        self$holdout <- dplyr::filter(data, .split_id == split_id)
        self$in_holdout <- data$.split_id == split_id
    }
))

#' Partition the data into folds
#'
#' This takes a dataset and a split ID and generates two subsets of the
#' data corresponding to a training set and a holdout.
#' @param data dataframe
#' @param split_id integer representing the split to construct
#' @return Returns an R6 object `HTEFold` with three public fields:
#' * `train` - The split to be used for training the plugin estimates
#' * `holdout` - The split not used for training
#' * `in_holdout` - A logical vector indicating for each unit whether they lie in the holdout.
#' @keywords internal
split_data <- function(data, split_id) {
    HTEFold$new(data, split_id)
}

#' R6 class to represent data to be used in estimating a model
#'
#' This class provides consistent names and interfaces to data which will
#' be used in a supervised regression / classification model.
#' @importFrom stats model.matrix model.frame
#' @importFrom purrr map
#' @export
Model_data <- R6::R6Class("Model_data", list(
    #' @field label The labels for the eventual model as a vector.
    label = NULL,
    #' @field features The matrix representation of the data to be used for model fitting.
    #' Constructed using `stats::model.matrix`.
    features = NULL,
    #' @field model_frame The data-frame representation of the data as constructed by
    #' `stats::model.frame`.
    model_frame = NULL,
    #' @field split_id The split identifiers as a vector.
    split_id = NULL,
    #' @field num_splits The integer number of splits in the data.
    num_splits = NULL,
    #' @field cluster A cluster ID as a vector, constructed using the unit identifiers.
    cluster = NULL,
    #' @field weights The case-weights as a vector.
    weights = NULL,
    #' @description
    #' Creates an R6 object to represent data to be used in a prediction model.
    #' @param data The full dataset to populate the class with.
    #' @param label_col  The unquoted name of the column to use as the label in
    #' supervised learning models.
    #' @param ... The unquoted names of features to use in the model.
    #' @param .weight_col The unquoted name of the column to use as case-weights
    #' in subsequent models.
    #' @return A `Model_data` object.
    #' @examples
    #' library("dplyr")
    #' df <- dplyr::tibble(
    #'     uid = 1:100,
    #'     x1 = rnorm(100),
    #'     x2 = rnorm(100),
    #'     x3 = sample(4, 100, replace = TRUE)
    #' ) %>% dplyr::mutate(
    #'     y = x1 + x2 + x3 + rnorm(100),
    #'     x3 = factor(x3)
    #' )
    #' df <- make_splits(df, uid, .num_splits = 5)
    #' data <- Model_data$new(df, y, x1, x2, x3)
    initialize = function(data, label_col, ..., .weight_col = NULL) {
        label_col <- rlang::enexpr(label_col)
        .weight_col <- rlang::enexpr(.weight_col)
        dots <- rlang::enexprs(...)

        self$label <- unlist(dplyr::select(data, {{ label_col }}))
        self$features <- model.matrix(~. + 0, data = dplyr::select(data, !!!dots))
        if (is.null(.weight_col)) {
            self$weights <- rep(1, length(self$label))
        } else {
            self$weights <- unlist(dplyr::select(data, {{ .weight_col }}))
            self$weights <- self$weights / sum(self$weights) * length(self$weights)
        }
        self$model_frame <- model.frame(~. + 0, data = dplyr::select(data, !!!dots))
        self$cluster <- data[[attr(data, "identifier")]]
        if (".split_id" %in% names(data)) {
            self$split_id <- data$.split_id
            self$num_splits <- attr(data, "num_splits")
        }
    },
    #' @description
    #' A helper function to create the cross-validation options to be used by SuperLearner.
    #' @seealso [SuperLearner::SuperLearner.CV.control]
    SL_cv_control = function() {
        validRows <- purrr::map(sort(unique(self$split_id)), ~which(.x == self$split_id))
        SuperLearner::SuperLearner.CV.control(V = length(validRows), validRows = validRows)
    }
))

#' Checks that splits have been properly created.
#'
#' This helper function makes a few simple checks to identify obvious
#' issues with the way that splits have been made in the supplied data.
#' @param data Dataframe which should have appropriate `.split_id` column.
#' @return Returns NULL. Errors if a problem is discovered.
#' @keywords internal
check_splits <- function(data) {
    attrs <- names(attributes(data))
    ok <- TRUE
    if (!(".split_id" %in% names(data))) ok <- FALSE
    if (!("num_splits" %in% attrs)) ok <- FALSE
    if (!("identifier" %in% attrs)) ok <- FALSE

    if (!ok) stop("You must first construct splits with `tidyhte::make_splits`.")
}

#' Checks that nuisance models have been estimated and exist in the supplied dataset.
#'
#' This helper function makes a few simple checks to identify obvious
#' issues with the way that nuisance functions are created and prepared.
#' @param data Dataframe which should have appropriate columns of nuisance function
#' predictions: `.pi_hat`, `.mu0_hat`, and `.mu1_hat`
#' @return Returns NULL. Errors if a problem is discovered.
#' @keywords internal
check_nuisance_models <- function(data) {
    cols <- names(data)
    ok <- TRUE
    if (!(".pi_hat" %in% cols)) ok <- FALSE
    if (!(".mu0_hat" %in% cols)) ok <- FALSE
    if (!(".mu1_hat" %in% cols)) ok <- FALSE

    if (!ok) stop("You must first estimate plugin models with `tidyhte::produce_plugin_estimates`.")
}

#' Removes rows which have missing data on any of the supplied columns.
#'
#' This function removes rows with missingness based on the columns provided.
#' If rows are dropped, a message is displayed to the user to inform them of this
#' fact.
#' @param data The dataset from which to drop cases which are not fully observed.
#' @param ... Unquoted column names which must be non-missing. Missingness in these
#' columns will result in dropped observations. Missingness in other columns will not.
#' @return The original data with all observations which are fully observed.
#' @importFrom stats complete.cases
#' @keywords internal
listwise_deletion <- function(data, ...) {
    dots <- rlang::enexprs(...)
    start_rows <- nrow(data)
    ok <- stats::complete.cases(dplyr::select(data, !!!dots))
    data <- data[ok, ]
    end_rows <- nrow(data)

    if (end_rows < start_rows) {
        dropped <- start_rows - end_rows
        msg <- paste0(
            "Dropped ",
            dropped,
            " of ",
            start_rows,
            " rows (",
            round(dropped / start_rows * 100, digits = 1),
            "%) through listwise deletion."
        )
        message(msg)
    }
    data
}

#' Checks that an appropriate identifier has been provided
#'
#' This helper function makes a few simple checks to identify obvious
#' issues with the way provided column of unit identifiers.
#' @param data Dataframe of interest.
#' @param id_col Quoted name of identifier column.
#' @return Returns NULL. Errors if a problem is discovered.
#' @keywords internal
check_identifier <- function(data, id_col) {
    N <- nrow(data)
    ids <- data[[id_col]]
    ok <- TRUE
    if (is.null(ids)) ok <- FALSE
    if (length(ids) != N) ok <- FALSE
    if (any(is.na(ids))) ok <- FALSE

    is_int <- checkmate::test_integerish(ids)
    is_fct <- checkmate::test_factor(ids)
    is_chr <- checkmate::test_character(ids)
    is_dbl <- checkmate::test_double(ids)

    if (!(is_int || is_fct || is_chr) && is_dbl) ok <- FALSE

    if (!ok) {
        msg <- "Invalid identifier. Each unit / cluster must have its own unique ID."
        stop(msg)
    }
}

#' Checks that an appropriate weighting variable has been provided
#'
#' This helper function makes a few simple checks to identify obvious
#' issues with the weights provided.
#' @param data Dataframe of interest.
#' @param weight_col Quoted name of weights column.
#' @return Returns NULL. Errors if a problem is discovered.
#' @keywords internal
check_weights <- function(data, weight_col) {
    if (!(weight_col %in% names(data))) {
        msg <- "Invalid weight column. Must exist in dataframe."
        stop(msg)
    }

    wts <- data[[weight_col]]

    is_num <- checkmate::test_numeric(wts)

    if (!is_num) {
        msg <- "Invalid weight column. Must be numeric."
        stop(msg)
    }

    if (min(wts) < 0) {
        msg <- "Invalid weight column. Must be non-negative."
        stop(msg)
    }
}

#' Checks that a dataframe has an attached configuration for HTEs
#'
#' This helper function ensures that the provided dataframe has
#' the necessary auxilliary configuration information for HTE
#' estimation.
#' @param data Dataframe of interest.
#' @return Returns NULL. Errors if a problem is discovered.
#' @keywords internal
check_data_has_hte_cfg <- function(data) {
    if (! "HTE_cfg" %in% names(attributes(data))) {
        msg <- "Must attach HTE_cfg with `attach_config`."
        stop(msg)
    }
}
