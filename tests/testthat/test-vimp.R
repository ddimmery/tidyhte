test_that("vimp works when quickblock isn't installed", {
    library(SuperLearner)
    df <- dplyr::tibble(
        uid = 1:100,
        x = rnorm(100),
        y = rnorm(100),
        a = rnorm(100)
    )
    attr(df, "identifier") <- "uid"
    sl_cfg <- SLEnsemble_cfg$new(learner_cfgs = list(SLLearner_cfg$new("SL.glm")))
    vimp_cfg <- VIMP_cfg$new(model_cfg = sl_cfg)
    expect_message(
        mockr::with_mock(
            package_present = function(x) FALSE,
            calculate_vimp(df, y, x, .VIMP_cfg = vimp_cfg)
        ),
        "`quickblock` is not installed, so falling back to un-stratified CV for VIMP."
    )
})
