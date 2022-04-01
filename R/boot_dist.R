#' boot_dist
#'
#' For a given dataframe `df`, generate `n_boot` bootstrap samples. For each
#' bootstrap sample, compute correlations between x and delta, and between x
#' and y. Slso compute the variance ratio.
#'
#' @param df input data frame. should have columns `x`, `y`, and `delta`
#' @param n_boot number of bootstrap samples
#'
#' @return a dataframe containing correlations and the variance ratio for each
#' bootstrap sample
#' @importFrom magrittr "%>%"
#' @import dplyr
#' @import tidyr
#' @import modelr
#' @import tibble
#' @export
#' @examples
#'
#'boot_dist(real_dfs,n_boot = 1)
boot_dist = function(df, n_boot = 250) {
  df %>%
    modelr::bootstrap(n_boot) %>%
    mutate(as_df = map(strap, as_tibble)) %>%
    select(-strap) %>%
    unnest(as_df) %>%
    group_by(.id) %>%
    summarize(c_xd = cor(x, delta), c_xy = cor(x, y), k = var(y) / var(x))
}

