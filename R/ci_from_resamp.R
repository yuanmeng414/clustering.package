#' ci_from_resamp
#'
#' Input is a dataframe containing results of a resampling analysis to look
#' at correlations between x and y, and between x and delta, as well as the
#' variance ratio. This returns confidence intervals for those values based on
#' the resampling results.
#'
#' @param df input dataframe. Contains estimates of correlations and k based
#' on repeated sampling.
#' @param method method to derive CI. For `quant`, uses quantiles. For
#' `t_quant`, uses a t-distribution adjusted quantile.
#' @param n See previous.
#'
#' @return
#' @importFrom magrittr "%>%"
#' @import dplyr
#' @import tidyr
#' @import modelr
#' @import tibble
#' @export
#'
#' @examples
#'boot_analysis = real_dfs %>%
#'filter(severe == FALSE) %>%
#'nest(-name) %>%
#'mutate(boot = map(data, boot_dist, n_boot = 1000)) %>%
#'select(-data)
#'ci_from_resamp(boot_analysis[[2]][[2]], method = "quant", 1)
ci_from_resamp = function(df, method = "quant", n) {

  if (method == "quant") {

    df %>%
      summarize(
        c_xy_lb = quantile(c_xy, .025), c_xy_ub = quantile(c_xy, .975),
        c_xd_lb = quantile(c_xd, .025), c_xd_ub = quantile(c_xd, .975),
        k_lb =    quantile(k, 0.025),   k_ub =    quantile(k, .975))

  } else if (method == "t_quant") {

    alpha_D2 = pnorm(sqrt(n / (n - 1)) * qt(0.025, df = n - 1))

    df %>%
      summarize(
        c_xy_lb = quantile(c_xy, alpha_D2), c_xy_ub = quantile(c_xy, 1 - alpha_D2),
        c_xd_lb = quantile(c_xd, alpha_D2), c_xd_ub = quantile(c_xd, 1 - alpha_D2),
        k_lb =    quantile(k, alpha_D2),   k_ub =    quantile(k, 1 - alpha_D2))
  }

}
