
#' gap_proc
#'
#' The clustering analyses make extensive use of the gap statistic, which is
#' implemented using cluster::clusGap(). The output of that function is
#' "untidy", and the purpose of this function is to clean output for subsequent
#' analysis and comparison.
#'
#' @param gap an object produced by cluster::clusGap()
#'
#' @return a tidied dataframe containing the same results as the input object.
#' @importFrom magrittr "%>%"
#' @import dplyr
#' @import tidyr
#' @import modelr
#' @export
#' @examples
#' seed = 1:5
#' winters_df =
#'real_dfs %>%
#'filter(name == "Winters", x != 66) %>%
#'mutate(
#'ii = 66 - x,
#'delta = y  - x,
#'prop = delta / (66 - x))
#' random_df = map(seed, random_recov_data, start_df = winters_df)
#' clust_df = map(random_df, ~select(.x, x, y))
#' gap_fit = map(.x = clust_df, ~cluster::clusGap(.x, FUN = hclust_fun, K.max = 5, B = 50, verbose = FALSE, spaceH0 = "original"))
#' gap_stat = map(gap_fit, gap_proc)
gap_proc = function(gap) {

  tibble::as_tibble(gap$Tab) %>%
  mutate(k = row_number()) %>%
  select(k, everything())

}
