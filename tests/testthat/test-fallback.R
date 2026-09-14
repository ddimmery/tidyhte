set.seed(20051920)

n <- 300
data <- dplyr::tibble(uid = 1:n) %>%
  dplyr::mutate(
    a = rbinom(n, 1, 0.5),
    ps = rep(0.5, n),
    x1 = rnorm(n),
    x2 = factor(sample(1:3, n, replace = TRUE)),
    x3 = rnorm(n),
    y = a + x1 + 0.5 * a * x1 + as.double(x2) + rnorm(n)
  )

# Pretend that no optional package is installed for the rest of the calling test.
mock_no_suggests <- function(.env = parent.frame()) {
  testthat::local_mocked_bindings(
    package_present = function(package) FALSE,
    .package = "tidyhte",
    .env = .env
  )
}

test_that("basic_config falls back to GLMs with a warning when SuperLearner is absent", {
  mock_no_suggests()
  expect_warning(
    cfg <- basic_config(),
    class = "tidyhte_warning_sl_fallback"
  )
  for (slot in c("treatment", "outcome", "effect")) {
    checkmate::expect_r6(cfg[[slot]], classes = c("GLM_cfg", "Model_cfg"))
  }
  expect_true(cfg$qoi$vimp$linear)
  expect_false(any(c("SL_risk", "SL_coefs") %in% cfg$qoi$diag$ps))
  expect_false(any(c("SL_risk", "SL_coefs") %in% cfg$qoi$diag$outcome))
  expect_false(any(c("SL_risk", "SL_coefs") %in% cfg$qoi$diag$effect))
  expect_true(all(c("AUC", "MSE") %in% cfg$qoi$diag$ps))
})

test_that("basic_config warns exactly once per call", {
  mock_no_suggests()
  warnings <- character()
  withCallingHandlers(
    basic_config(),
    tidyhte_warning_sl_fallback = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(warnings, 1)
})

test_that("HTE_cfg defaults fall back to GLMs with a warning", {
  mock_no_suggests()
  qoi <- QoI_cfg$new(ate = TRUE, diag = Diagnostics_cfg$new(ps = "MSE"))
  expect_warning(
    cfg <- HTE_cfg$new(qoi = qoi),
    class = "tidyhte_warning_sl_fallback"
  )
  for (slot in c("treatment", "outcome", "effect")) {
    checkmate::expect_r6(cfg[[slot]], "GLM_cfg")
  }
  # No warning when every model is provided explicitly.
  expect_no_warning(
    HTE_cfg$new(
      treatment = Known_cfg$new("ps"), outcome = GLM_cfg$new(), effect = GLM_cfg$new(), qoi = qoi
    )
  )
})

test_that("default_model_cfg follows package availability", {
  local({
    mock_no_suggests()
    checkmate::expect_r6(default_model_cfg(), "GLM_cfg")
  })
  skip_if_no_superlearner()
  checkmate::expect_r6(default_model_cfg(), "SLEnsemble_cfg")
})

test_that("the full pipeline runs with GLM nuisance models and no optional packages", {
  mock_no_suggests()
  cfg <- suppressWarnings(basic_config()) %>%
    add_moderator("Stratified", x2, x3)
  checkmate::expect_r6(cfg$treatment, "GLM_cfg")

  d <- attach_config(data, cfg) %>% make_splits(uid, .num_splits = 4)
  expect_warning(
    d <- produce_plugin_estimates(d, y, a, x1, x2, x3),
    class = "tidyhte_warning_glm_model"
  )
  expect_true(all(c(".pi_hat", ".mu1_hat", ".mu0_hat") %in% names(d)))
  expect_false(any(is.na(d$.mu1_hat)))
  d <- construct_pseudo_outcomes(d, y, a)
  # Without PCATEs or predictions the effect model is never fit, so no GLM warning here.
  expect_no_warning(result <- estimate_QoI(d, x2, x3), class = "tidyhte_warning_glm_model")
  checkmate::expect_data_frame(result)
  expect_true(all(c("SATE", "MCATE", "VIMP", "MSE") %in% result$estimand))
  # AUC is skipped (with a message) because `WeightedROC` is treated as absent.
  expect_false("AUC" %in% result$estimand)
  # No spurious rows from diagnostics that could not be computed.
  expect_false(any(is.na(result$estimate)))
  ate <- dplyr::filter(result, estimand == "SATE")
  expect_gt(ate$estimate, 0.5)
  expect_lt(ate$estimate, 1.5)

  # The effect model (only fit for PCATEs / predictions) warns as well.
  expect_warning(
    fx <- fit_fx_predictor(
      d, .weights, ".pseudo_outcome", x1, x2, x3,
      .pcate.cfg = NULL, .Model_cfg = cfg$effect
    ),
    class = "tidyhte_warning_glm_model"
  )
  expect_false(any(is.na(fx$data$.pseudo_outcome_hat)))
})

test_that("the pipeline with a known propensity score and GLM models needs no AUC package", {
  mock_no_suggests()
  cfg <- suppressWarnings(basic_config()) %>%
    add_known_propensity_score("ps")
  d <- attach_config(data, cfg) %>% make_splits(uid, .num_splits = 4)
  expect_warning(
    d <- produce_plugin_estimates(d, y, a, x1, x2, x3),
    class = "tidyhte_warning_glm_model"
  )
  d <- construct_pseudo_outcomes(d, y, a)
  # Without PCATEs or predictions the effect model is never fit, so no GLM warning.
  expect_no_warning(result <- estimate_QoI(d, x1, x3), class = "tidyhte_warning_glm_model")
  checkmate::expect_data_frame(result)
  expect_true(all(c("SATE", "VIMP", "MSE") %in% result$estimand))
  expect_false("AUC" %in% result$estimand)
})

test_that("AUC diagnostic degrades with a message when WeightedROC is absent", {
  mock_no_suggests()
  df <- dplyr::tibble(y = rbinom(100, 1, 0.5), p = rep(0.5, 100), w = rep(1, 100))
  attr(df, "weights") <- "w"
  expect_message(
    result <- calculate_auc_diagnostic(df, "y", "p"),
    "`WeightedROC` is not installed"
  )
  expect_null(result)
})
