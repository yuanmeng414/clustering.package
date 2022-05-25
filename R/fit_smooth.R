#' fit_smooth
#'
#' @param formula
#' @param data
#' @param thresh
#'
#' @return
#' @export
#'
#' @examples
#' fit_smooth(y_obs ~ x, data = df)
fit_smooth = function(formula, data, thresh = 66){
  mgcv::gam(y_obs ~ s(x), data = data)
}
