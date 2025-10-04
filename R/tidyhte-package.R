#' @details
#' The best place to get started with `tidyhte` is `vignette("experimental_analysis")` which
#' walks through a full analysis of HTE on simulated data, or `vignette("methodological_details")`
#' which gets into more of the details underlying the method.
#' @seealso The core public-facing functions are `make_splits`, `produce_plugin_estimates`,
#' `construct_pseudo_outcomes` and `estimate_QoI`. Configuration is accomplished through `HTE_cfg`
#' in addition to a variety of related classes (see `basic_config`).
#' @references Kennedy, E. H. (2023). Towards optimal doubly robust estimation of heterogeneous
#' causal effects. *Electronic Journal of Statistics*, 17(2), 3008-3049.
#' @keywords internal
"_PACKAGE"

if (getRversion() >= "2.15.1") utils::globalVariables(".data")
