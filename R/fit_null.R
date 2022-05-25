#' fit_null
#'
#' @param formula
#' @param data
#' @param thresh
#'
#' @return
#' @export
#'
#' @examples
#' fit_null(y_obs ~ x, data = df)
fit_null = function(formula, data, thresh = 66){
  lm(y_obs ~ 1, data = data)
}
