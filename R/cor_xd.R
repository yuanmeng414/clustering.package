#' cor_xd
#'
#'function computes cor_xd from k and cor_xy. used especially for creating plots.
#' @param k
#' @param cor_xy
#' @return plots
#' @importFrom magrittr "%>%"
#' @import dplyr
#' @import tidyr
#' @import modelr
#' @export
#'
#' @examples
#'
#' cor_xd(0.1, -1)
cor_xd = function(k, cor_xy) {

	(-k + cor_xy) / sqrt(k^2 + 1 - 2 * k * cor_xy)

}

