set.seed(20051920)

n <- 200
df <- dplyr::tibble(
  uid = 1:n,
  x1 = rnorm(n),
  x2 = rnorm(n),
  x3 = factor(sample(4, n, replace = TRUE)),
  w = rexp(n) + 0.1
) %>% dplyr::mutate(
  y = x1 + x2 + as.integer(x3) + rnorm(n),
  a = rbinom(n, 1, plogis(x1))
)
df <- make_splits(df, uid, .num_splits = 4)

test_that("GLM_cfg is a Model_cfg with a family", {
  cfg <- GLM_cfg$new()
  checkmate::expect_r6(cfg, classes = c("GLM_cfg", "Model_cfg"))
  expect_equal(cfg$model_class, "GLM")
  expect_equal(cfg$family$family, "gaussian")
  cfg <- GLM_cfg$new(family = stats::binomial())
  expect_equal(cfg$family$family, "binomial")
})

test_that("GLMPredictor fits and predicts with the expected shape", {
  pred <- predictor_factory(GLM_cfg$new())
  data <- Model_data$new(df, y, x1, x2, x3, .weight_col = w)
  expect_no_warning(pred$fit(data))
  expect_no_warning(o <- pred$predict(data))
  checkmate::expect_data_frame(o, nrows = n)
  checkmate::expect_names(names(o), permutation.of = c("x", "estimate", "sample_size"))
  expect_true(all(is.na(o$x)))
  expect_true(all(o$sample_size == 1))
  expect_true(cor(o$estimate, df$y) > 0.7)

  # Weighted fit reproduces stats::glm with the same (normalized) weights.
  ref <- stats::glm(y ~ x1 + x2 + x3, data = df, weights = df$w / sum(df$w) * n)
  expect_equal(unname(o$estimate), unname(stats::predict(ref, type = "response")))
})

test_that("GLMPredictor returns the covariate as x for a single feature", {
  pred <- predictor_factory(GLM_cfg$new())
  data <- Model_data$new(df, y, x1)
  pred$fit(data)
  o <- pred$predict(data)
  expect_equal(o$x, df$x1, ignore_attr = TRUE)
  expect_true(cor.test(o$x, o$estimate)$p.value < 0.05)
})

test_that("GLMPredictor zero-fills features missing at prediction time", {
  pred <- predictor_factory(GLM_cfg$new())
  pred$fit(Model_data$new(df, y, x1, x2, x3))
  newdata <- Model_data$new(dplyr::filter(df, x3 == "1"), y, x1, x2, x3)
  expect_no_warning(o <- pred$predict(newdata))
  checkmate::expect_data_frame(o, nrows = sum(df$x3 == "1"))
  expect_false(any(is.na(o$estimate)))
})

test_that("GLMPredictor respects a binomial family with non-integer weights", {
  pred <- predictor_factory(GLM_cfg$new(family = stats::binomial()))
  data <- Model_data$new(df, a, x1, x2, .weight_col = w)
  expect_no_warning(pred$fit(data))
  o <- pred$predict(data)
  expect_true(all(o$estimate > 0 & o$estimate < 1))
  expect_true(cor(o$estimate, df$a) > 0.2)
})
