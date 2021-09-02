test_that("SL model slot", {
    expect_error(SL_model_slot("test"), "Unknown model slot.")

    checkmate::expect_character(SL_model_slot(".pi_hat"), pattern = "^pi$")

    checkmate::expect_character(SL_model_slot(".mu1_hat"), pattern = "^mu1$")

    checkmate::expect_character(SL_model_slot(".mu0_hat"), pattern = "^mu0$")
})


test_that("estimate_diagnostic", {
    df <- dplyr::tibble(y = rnorm(100), p = rep(0.5, 100))
    expect_message(
        estimate_diagnostic(df, "y", "p", "AUC"),
        "Cannot calculate AUC because labels are not binary."
    )

    expect_message(
        estimate_diagnostic(df, "y", "p", "SL_risk"),
        "Cannot calculate SL_risk because the model is not SuperLearner."
    )

    expect_message(
        estimate_diagnostic(df, "y", "p", "SL_coefs"),
        "Cannot calculate SL_coefs because the model is not SuperLearner."
    )
})
