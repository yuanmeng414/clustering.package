#' predict.fit_const
#' This function predict the constant model
#' @param object
#' @param data.new
#'
#' @return
#' @export
#'
#' @examples
predict.fit_const = function(object, data.new = NULL) {

  if (is.null(data.new)) {data.new = object$data}

  pred = object$const_est + data.new$x
  pred = ifelse(pred < object$thresh, pred, object$thresh)

  pred
}
