#' predict.fit_prr
#' This function predict the proportion recovery model
#' @param object
#' @param data.new
#'
#' @return
#' @export
#'
#' @examples
predict.fit_prr = function(object, data.new = NULL) {

  object = object %>%
    mutate(delta_obs = y_obs -x, fmii = 66 - x)

  if (is.null(data.new)) {data.new = object}


  pred_delta = lm(delta_obs ~ 0 + fmii, data = data.new)
  pred_y = data.new$fmii + object$delta_obs

  pred_y
}

