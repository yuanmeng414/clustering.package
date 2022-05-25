#' fit_exp
#'
#' @param formula
#' @param data
#' @param thresh
#'
#' @return
#' @export
#'
#' @examples
#' fit_exp(y_obs ~ x, data = df)
fit_exp = function(formula, data, thresh = 66) {
  nls(y_obs ~ a + r * (66 - a) * (1 - exp(-x / tau)), data = data,
      algorithm = "port", control = list(maxiter = 10000, warnOnly = TRUE),
      start = list(a = 30, r = 0.95, tau = 20), lower = c(0, 0, 0), upper = c(66, 1, Inf))
  model_const = fit_const(y_obs ~ x, data = data)

}
