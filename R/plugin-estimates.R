
Predictor = R6::R6Class("Predictor",
    list(
        predict=function(X) {
            predict(self$model, X)
        }
    )
)

pass_through_model <- function(.data, cov) {
    ps_mod = function(.data) .data[[cov]]
    ps = ps_mod(.data)
    list(pred=ps, fit=ps_mod)
}

fit.plugin.A <- function(.data, a_col, ..., .SL.cfg) {
    # short-circuit if the true model is known
    if(.SL.library == "known") {
        return(pass_through_model(.data, .SL.cfg$covariate_name))
    }

    data = prepare_model_data(.data, a_col, ...)
    SuperLearner(Y = data$label, X = data$features, family = binomial(), SL.library = .SL.cfg$SL.library)
}

fit.plugin.Y <- function(.data, y_col, a_col, ..., .SL.cfg) {
    df_0 = .data %>% filter({{a_col}} == 0)
    df_1 = .data %>% filter({{a_col}} == 1)

    data0 = prepare_model_data(df_0, y_col, ...)
    data1 = prepare_model_data(df_1, y_col, ...)
    list(
        mu1=SuperLearner(Y = data0$label, X = data0$features, SL.library = SL.library),
        mu0=SuperLearner(Y = data1$label, X = data1$features, SL.library = SL.library)
    )
}

# tidier api here plz
fit.effect <- function(pseudo_outcome, X, .SL.cfg) {
    SuperLearner(Y = pseudo_outcome, X = X, SL.library = .SL.cfg$SL.library)
}


predict.plugin.Y <- function(Y) {
    return(rnorm(1))
}