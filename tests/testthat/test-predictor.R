


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
