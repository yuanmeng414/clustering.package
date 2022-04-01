#' random_recov_data
#'
#' Given a starting dataset, this function extracts observed baseline values
#' and, given these, simulates follow-up values between the baseline and 66
#' (the max allowed value) using a uniform distribution. The mechanism is
#' intended to mimic the random recovery process implemented in various papers.
#' This function is used in the context of clustering analysis and simulations.
#'
#' @param seed seed for reproducibility
#' @param start_df data set that is used as the basis for data simulated under
#' random recovery.
#'
#' @return dataframe containing randomly generated data
#' @importFrom magrittr "%>%"
#' @import dplyr
#' @import tidyr
#' @import modelr
#' @export
#'
#' @examples
#'winters_df = real_dfs %>%
#'filter(name == "Winters", x != 66) %>%
#'mutate(
#'ii = 66 - x,
#'delta = y  - x,
#'prop = delta / (66 - x))
#' random_recov_data(1, winters_df)
random_recov_data = function(seed, start_df) {

  set.seed(seed)

  x_vals =
    start_df %>%
    pull(x)

  n = length(x_vals)

  random_df =
    tibble::tibble(
      x = x_vals,
      ii = 66 - x,
      y = runif(n, x, 66),
      delta = y - x,
      prop = delta / (66 - x),
      severe = FALSE
    )

  random_df
}
