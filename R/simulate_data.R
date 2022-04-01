#' simulate_data
#'
#' @description This function is used to simulate data illustrative of the
#' relationship between cor_xy, cor_xd, and the variance ratio. This function
#' is used to generate datasets A-E (dataset E is subjected to a ceiling
#' effect prior to analysis).
#'
#' @param n sample size
#' @param seed seed for reproducibility
#' @param mu_x mean of the $x$ observations
#' @param mu_y mean of the $y$ observations
#' @param var_x variance of the $x$ observations
#' @param var_ratio variance ratio (`var_y` / `var_x`)
#' @param cor_xy correlation betwene $x$ and $y$
#' @param empirical argument to `MASS::mvrnorm` -- should variances and correlations
#' be exactly as specified, or take these values on average?
#' @param ... additional arguments
#'
#' @return dataframe containing simulated generated data
#' @importFrom magrittr "%>%"
#' @import dplyr
#' @import tidyr
#' @import modelr
#' @export
#'
#' @examples
#'
#' simulate_data(n = 30, seed = 1,
#'              mu_x = 30, mu_y = 30,
#'             var_x = 100, var_ratio = 1, cor_xy = 0,
#'             empirical = TRUE)
#'
simulate_data = function(n = 30, seed = 1,
												 mu_x = 30, mu_y = 30,
												 var_x = 100, var_ratio = 1, cor_xy = 0,
												 empirical = TRUE, ...) {

	set.seed(seed)

	var_y = var_x * var_ratio
	cov_xy = cor_xy * sqrt(var_x) * sqrt(var_y)

	Sigma = matrix(c(var_x, cov_xy, cov_xy, var_y), 2, 2)

	obs_data =
		MASS::mvrnorm(n, mu = c(mu_x, mu_y), Sigma = Sigma, empirical = empirical)

	colnames(obs_data) = c("x", "y")

	df =
		tibble::as_tibble(obs_data) %>%
	  mutate(
		  subj = 1:n,
  		delta = y - x
	  ) %>%
	  select(subj, x, y, delta)

	df
}

