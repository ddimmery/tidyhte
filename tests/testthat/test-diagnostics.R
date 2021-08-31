test_that("SL model slot", {
    expect_error(SL_model_slot("test"), "Unknown model slot.")

    checkmate::expect_character(SL_model_slot(".pi_hat"), pattern = "^pi$")

    checkmate::expect_character(SL_model_slot(".mu1_hat"), pattern = "^mu1$")

    checkmate::expect_character(SL_model_slot(".mu0_hat"), pattern = "^mu0$")
})