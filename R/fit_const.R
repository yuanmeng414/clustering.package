#' fit_const
#' This function fit the constant model
#' @param formula
#' @param data
#' @param thresh
#'
#' @return
#' @export
#'
#' @examples
#' fit_const(y_obs ~ x, data = df)
fit_const = function(formula, data, thresh = 66) {

  initmod = lm(formula, data = data)

  outcome.var = as.character(formula)[2]
  y = data %>% pull(outcome.var)

  X = model.matrix(initmod)
  init = c(coef(initmod)[1], log_sigma = log(summary(initmod)$sigma))

  param_est = optim(
    par = init,
    const_tobit_ll,
    y  = y,
    X  = X,
    ul = thresh,
    method  = 'BFGS',
    control = list(maxit = 2000, reltol = 1e-15)
  )

  ret = list(
    data = data,
    thresh = thresh,
    const_est = param_est$par[1]
  )

  class(ret) = "fit_const"

  ret
}
