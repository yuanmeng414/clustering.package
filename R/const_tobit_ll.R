#' const_tobit_ll
#' This function only takes a lower OR upper limit
#' @param par
#' @param X
#' @param y
#' @param ul
#' @param ll
#'
#' @return
#' @export
#'
#' @examples
#'
const_tobit_ll <- function(par, X, y, ul = -Inf, ll = Inf) {


  # parameters
  sigma = exp(par[2])
  const  = par[1]
  beta = c(const, 1)

  # create indicator depending on chosen limit
  if (!is.infinite(ll)) {
    limit = ll
    indicator = y > ll
  } else {
    limit = ul
    indicator = y < ul
  }

  # linear predictor
  lp = X %*% beta

  # log likelihood
  ll = sum(indicator * log((1/sigma)*dnorm((y-lp)/sigma)) ) +
    sum((1-indicator) * log(pnorm((lp-limit)/sigma, lower = is.infinite(ll))))

  -ll
}
