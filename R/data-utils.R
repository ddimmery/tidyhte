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
    cluster = NULL,
    weights = NULL,
    initialize = function(.data, label_col, ..., .weight_col = NULL) {
        label_col <- rlang::enexpr(label_col)
        .weight_col <- rlang::enexpr(.weight_col)
        dots <- rlang::enexprs(...)

        self$label <- unlist(dplyr::select(.data, {{ label_col }}))
        self$features <- model.matrix(~. + 0, data = dplyr::select(.data, !!!dots))
        if (is.null(.weight_col)) {
            self$weights <- rep(1, length(self$label))
        } else {
            self$weights <- unlist(dplyr::select(.data, {{ .weight_col }}))
        }
        self$model_frame <- model.frame(~. + 0, data = dplyr::select(.data, !!!dots))
        self$cluster <- .data[[attr(.data, "identifier")]]
        if (".split_id" %in% names(.data)) {
            self$split_id <- .data$.split_id
            self$num_splits <- attr(.data, "num_splits")
        }
    },
    SL_cv_control = function() {
        validRows <- purrr::map(unique(self$split_id), ~which(.x == self$split_id))
        SuperLearner::SuperLearner.CV.control(V = length(validRows), validRows = validRows)
    }
))

check_splits <- function(data) {
    attrs <- names(attributes(data))
    ok <- TRUE
    if (!(".split_id" %in% names(data))) ok <- FALSE
    if (!("num_splits" %in% attrs)) ok <- FALSE
    if (!("identifier" %in% attrs)) ok <- FALSE

    if (!ok) stop("You must first construct splits with `tidyhte::make_splits`.")
}

check_nuisance_models <- function(data) {
    cols <- names(data)
    ok <- TRUE
    if (!(".pi_hat" %in% cols)) ok <- FALSE
    if (!(".mu0_hat" %in% cols)) ok <- FALSE
    if (!(".mu1_hat" %in% cols)) ok <- FALSE

    if (!ok) stop("You must first estimate plugin models with `tidyhte::produce_plugin_estimates`.")
}


#' @importFrom stats complete.cases
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

    if (!(is_int | is_fct | is_chr) & is_dbl) ok <- FALSE

    if (!ok) {
        msg <- "Invalid identifier. Each unit / cluster must have its own unique ID."
        stop(msg)
    }
}

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

    if(min(wts) < 0) {
        msg <- "Invalid weight column. Must be non-negative."
        stop(msg)
    }
}

check_data_has_hte_cfg <- function(data) {
    if (! "HTE_cfg" %in% names(attributes(data))) {
        msg <- "Must attach HTE_cfg with `attach_config`."
        stop(msg)
    }
}
