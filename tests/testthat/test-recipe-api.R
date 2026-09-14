test_that("basic config shortcut", {
  cfg <- suppressWarnings(basic_config())
  checkmate::expect_r6(cfg, "HTE_cfg")
  for (slot in c("treatment", "outcome", "effect")) {
    checkmate::expect_r6(cfg[[slot]], "Model_cfg")
  }
  checkmate::expect_r6(cfg$qoi$vimp, "VIMP_cfg")
})

test_that("basic config uses SuperLearner ensembles when available", {
  skip_if_no_superlearner()
  expect_no_warning(cfg <- basic_config())
  for (slot in c("treatment", "outcome", "effect")) {
    checkmate::expect_r6(cfg[[slot]], "SLEnsemble_cfg")
    expect_equal(cfg[[slot]]$SL.library, "SL.glm")
  }
  expect_true(all(c("SL_risk", "SL_coefs") %in% cfg$qoi$diag$ps))
  expect_true(all(c("SL_risk", "SL_coefs") %in% cfg$qoi$diag$outcome))
  expect_true(all(c("SL_risk", "SL_coefs") %in% cfg$qoi$diag$effect))
  expect_equal(cfg$qoi$vimp$linear, !rlang::is_installed("vimp"))
})

cfg <- suppressWarnings(basic_config())

test_that("recipe manipulations on ps", {
  skip_if_no_superlearner()
  checkmate::expect_r6(add_propensity_score_model(cfg, "SL.glmnet"), "HTE_cfg")
  checkmate::expect_r6(cfg$treatment, "SLEnsemble_cfg")
  expect_true("SL.glmnet" %in% cfg$treatment$SL.library)
})

test_that("recipe manipulations on ps (no optional packages)", {
  checkmate::expect_r6(add_known_propensity_score(cfg, "pscore"), "HTE_cfg")
  checkmate::expect_r6(cfg$treatment, "Known_cfg")
  expect_false("AUC" %in% cfg$qoi$diag$ps)

  checkmate::expect_r6(add_propensity_diagnostic(cfg, "MSE"), "HTE_cfg")
})

test_that("recipe manipulations on outcome", {
  skip_if_no_superlearner()
  checkmate::expect_r6(add_outcome_model(cfg, "SL.glmnet"), "HTE_cfg")
  checkmate::expect_r6(cfg$outcome, "SLEnsemble_cfg")
  expect_true("SL.glmnet" %in% cfg$outcome$SL.library)
})

test_that("recipe manipulations on outcome (no optional packages)", {
  checkmate::expect_r6(add_outcome_diagnostic(cfg, "MSE"), "HTE_cfg")
})

test_that("recipe manipulations on effect", {
  skip_if_no_superlearner()
  checkmate::expect_r6(add_effect_model(cfg, "SL.glmnet"), "HTE_cfg")
  checkmate::expect_r6(cfg$effect, "SLEnsemble_cfg")
  expect_true("SL.glmnet" %in% cfg$effect$SL.library)
})

test_that("recipe manipulations on effect (no optional packages)", {
  checkmate::expect_r6(add_effect_diagnostic(cfg, "MSE"), "HTE_cfg")
})

test_that("recipe manipulations on moderators", {
  checkmate::expect_r6(add_moderator(cfg, "Stratified", x1), "HTE_cfg")

  expect_error(add_moderator(cfg, "unknown", x1), "Unknown `model_type`.")

  skip_if_not_installed("nprobust")
  checkmate::expect_r6(
    add_moderator(cfg, "KernelSmooth", x2, .model_arguments = rlang::list2(neval = 50)),
    "HTE_cfg"
  )

  checkmate::expect_r6(add_moderator(cfg, "KernelSmooth", x2), "HTE_cfg")
})

test_that("recipe manipulations on vimp", {
  checkmate::expect_r6(add_vimp(cfg, linear_only = TRUE), "HTE_cfg")
  expect_true(cfg$qoi$vimp$linear)
  expect_true(cfg$qoi$vimp$sample_splitting)

  checkmate::expect_r6(remove_vimp(cfg), "HTE_cfg")
  expect_null(cfg$qoi$vimp)

  skip_if_no_vimp()
  checkmate::expect_r6(add_vimp(cfg, sample_splitting = FALSE), "HTE_cfg")
  expect_false(cfg$qoi$vimp$linear)
  expect_false(cfg$qoi$vimp$sample_splitting)
})

test_that("init treatment", {
  skip_if_no_superlearner()
  cfg$treatment <- NULL
  checkmate::expect_r6(add_propensity_score_model(cfg, "SL.glmnet"), "HTE_cfg")
  checkmate::expect_r6(cfg$treatment, "SLEnsemble_cfg")
})

test_that("init outcome", {
  skip_if_no_superlearner()
  cfg$outcome <- NULL
  checkmate::expect_r6(add_outcome_model(cfg, "SL.glmnet"), "HTE_cfg")
  checkmate::expect_r6(cfg$outcome, "SLEnsemble_cfg")
})

test_that("adding SL models upgrades GLM configs and keeps the family", {
  skip_if_no_superlearner()
  glm_cfg <- HTE_cfg$new(
    treatment = GLM_cfg$new(family = stats::binomial()),
    outcome = GLM_cfg$new(),
    effect = GLM_cfg$new(),
    qoi = QoI_cfg$new(ate = TRUE, diag = Diagnostics_cfg$new(ps = "MSE"))
  )
  add_propensity_score_model(glm_cfg, "SL.glm")
  add_outcome_model(glm_cfg, "SL.glm")
  add_effect_model(glm_cfg, "SL.glm")
  for (slot in c("treatment", "outcome", "effect")) {
    checkmate::expect_r6(glm_cfg[[slot]], "SLEnsemble_cfg")
  }
  expect_equal(glm_cfg$treatment$family$family, "binomial")
  expect_equal(glm_cfg$outcome$family$family, "gaussian")
})
