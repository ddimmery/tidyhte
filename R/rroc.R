#' Regression ROC Curve calculation
#'
#' This function calculates the RegressionROC Curve of
#' of Hernández-Orallo
#' \doi{doi:10.1016/j.patcog.2013.06.014}.
#' It provides estimates for the positive and negative
#' errors when predictions are shifted by a variety
#' of constants (which range across the domain of observed
#' residuals). Curves closer to the axes are, in general, to be
#' preferred. In general, this curve provides a simple way to
#' visualize the error properties of a regression model.
#'
#' The dot shows the errors when no shift is applied, corresponding
#' to the base model predictions.
#' @param label True label
#' @param prediction Model prediction of the label (out of sample)
#' @param nbins Number of shift values to sweep over
#' @references Hernández-Orallo, J. (2013). ROC curves for regression.
#' Pattern Recognition, 46(12), 3395-3411.
#' @return A tibble with `nbins` rows.
#' @importFrom dplyr tibble bind_rows
#' @importFrom stats quantile
#' @keywords internal
calculate_rroc <- function(label, prediction, nbins = 100) {
    residuals <- label - prediction
    n <- length(residuals)

    shifts <- stats::quantile(residuals, probs = seq(0, 1, length.out = nbins - 1))

    result <- calculate_pos_and_neg(residuals, 0)
    results <- dplyr::tibble(
        estimand = "RROC",
        value = result$pos / n,
        level = "observed",
        estimate = result$neg / n,
        std_error = NA_real_
    )
    for (shift in shifts) {
        result <- calculate_pos_and_neg(residuals, -shift)
        results <- dplyr::bind_rows(
            results,
            dplyr::tibble(
                estimand = "RROC",
                value = result$pos / n,
                level = "shifted",
                estimate = result$neg / n,
                std_error = NA_real_
            )
        )
    }
    results
}

#' @noRd
#' @keywords internal
#' @importFrom rlang list2
calculate_pos_and_neg <- function(residuals, shift = 0.0) {
    shifted_residuals <- residuals + shift
    rlang::list2(
        pos = sum(shifted_residuals[shifted_residuals > 0]),
        neg = sum(shifted_residuals[shifted_residuals <= 0])
    )
}
