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

test_that("calculate_diagnostics drops diagnostics that cannot be computed", {
  df <- dplyr::tibble(
    y = rnorm(100), a = rbinom(100, 1, 0.5), w = rep(1, 100), u = 1:100,
    .pi_hat = rep(0.5, 100), .mu1_hat = rnorm(100), .mu0_hat = rnorm(100)
  )
  attr(df, "weights") <- "w"
  attr(df, "identifier") <- "u"
  attr(df, "treatment") <- "a"
  attr(df, "outcome") <- "y"
  attr(df, "SL_coefs") <- list(pi = list(), mu0 = list(), mu1 = list())
  diag_cfg <- Diagnostics_cfg$new(
    ps = c("MSE", "SL_risk", "SL_coefs"),
    outcome = c("MSE", "SL_coefs")
  )
  expect_message(
    result <- calculate_diagnostics(df, a, y, .diag.cfg = diag_cfg),
    "Cannot calculate SL_risk"
  )
  checkmate::expect_data_frame(result, nrows = 3)
  expect_true(all(result$estimand == "MSE"))
  expect_false(any(is.na(result$estimate)))
})
