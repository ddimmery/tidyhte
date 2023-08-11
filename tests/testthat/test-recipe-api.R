test_that("basic config shortcut", {
    cfg <- basic_config()
    checkmate::expect_r6(cfg, "HTE_cfg")
})

cfg <- basic_config()

test_that("recipe manipulations on ps", {
    checkmate::expect_r6(add_propensity_score_model(cfg, "SL.glmnet"), "HTE_cfg")

    checkmate::expect_r6(add_known_propensity_score(cfg, "pscore"), "HTE_cfg")

    checkmate::expect_r6(add_propensity_diagnostic(cfg, "MSE"), "HTE_cfg")
})

test_that("recipe manipulations on outcome", {
    checkmate::expect_r6(add_outcome_model(cfg, "SL.glmnet"), "HTE_cfg")

    checkmate::expect_r6(add_outcome_diagnostic(cfg, "MSE"), "HTE_cfg")
})

test_that("recipe manipulations on effect", {
    checkmate::expect_r6(add_effect_model(cfg, "SL.glmnet"), "HTE_cfg")

    checkmate::expect_r6(add_effect_diagnostic(cfg, "MSE"), "HTE_cfg")
})

test_that("recipe manipulations on moderators", {
    checkmate::expect_r6(add_moderator(cfg, "Stratified", x1), "HTE_cfg")

    expect_error(add_moderator(cfg, "unknown", x1), "Unknown `model_type`.")

    checkmate::expect_r6(
        add_moderator(cfg, "KernelSmooth", x2, .model_arguments = rlang::list2(neval = 50)),
        "HTE_cfg"
    )

    checkmate::expect_r6(add_moderator(cfg, "KernelSmooth", x2), "HTE_cfg")
})

test_that("recipe manipulations on vimp", {
    checkmate::expect_r6(add_vimp(cfg, sample_splitting = FALSE), "HTE_cfg")
})

test_that("init treatment", {
    cfg$treatment <- NULL
    checkmate::expect_r6(add_propensity_score_model(cfg, "SL.glmnet"), "HTE_cfg")
})

test_that("init outcome", {
    cfg$outcome <- NULL
    checkmate::expect_r6(add_outcome_model(cfg, "SL.glmnet"), "HTE_cfg")
})
