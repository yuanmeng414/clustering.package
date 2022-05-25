#' fit_prr
#'
#' @param formula
#' @param data
#' @param thresh
#'
#' @return
#' @export
#'
#' @examples
#' fit_prr(y_obs ~ x, data = df)
fit_prr = function(formula, data, thresh = 66) {
  data = data  %>%
    mutate(delta_obs = y_obs -x, fmii = 66 -x)
  lm(delta_obs ~ 0 + fmii, data = data)
}
