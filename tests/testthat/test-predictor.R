
test_that("predictor factory", {
    model_cfg <- SLEnsemble_cfg$new()
    checkmate::expect_r6(predictor_factory(model_cfg), classes = "Predictor")

    model_cfg <- Known_cfg$new("test")
    checkmate::expect_r6(predictor_factory(model_cfg), classes = "Predictor")

    model_cfg <- Model_cfg$new()
    expect_error(predictor_factory(model_cfg), "Unknown model class.")

    expect_error(predictor_factory(list(model_class = "test")), "Unknown model class.")

    expect_error(predictor_factory("test"), "Unknown model class.")
})

test_that("base Predictor class", {
    expect_warning(pred <- Predictor$new(), "Not Implemented")
    expect_error(pred$fit(y, x))
    expect_error(pred$predict(data))
    expect_error(pred$predict_se(data))
})

test_that("check check_nuisance_models", {
    df <- dplyr::tibble(a = 1:10, b = letters[1:10])
    expect_error(
        check_nuisance_models(df),
        "You must first estimate plugin models with `tidyhte::produce_plugin_estimates`."
    )
})

test_that("SL_predictor gives expected output", {
    slpred <- predictor_factory(
        SLEnsemble_cfg$new(learner_cfgs = list(
            SLLearner_cfg$new("SL.glm"), SLLearner_cfg$new("SL.gam")
        ))
    )
    df <- dplyr::tibble(
        uid = 1:100,
        x1 = rnorm(100),
        x2 = rnorm(100),
        x3 = sample(4, 100, replace = TRUE)
    ) %>% dplyr::mutate(
        y = x1 + x2 + x3 + rnorm(100),
        x3 = factor(x3)
    )
    df <- make_splits(df, uid, .num_splits = 5)
    data <- Model_data$new(df, y, x1, x2, x3)
    slpred$fit(data)
    expect_error(o <- slpred$predict(data), NA)
    checkmate::expect_data_frame(o, nrows = 100)
    expect_true(all(is.na(o$x)))

    data <- Model_data$new(df, y, x1)
    slpred$fit(data)
    expect_error(o <- slpred$predict(data), NA)
    checkmate::expect_data_frame(o, nrows = 100)
    expect_true(cor.test(o$x, o$estimate)$p.value < 0.05)
    expect_true(all(!is.na(o$x)))
    expect_equal(o$x, df$x1, ignore_attr = TRUE)
})
