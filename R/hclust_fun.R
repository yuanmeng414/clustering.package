#' hclust_fun
#'
#' This function is a wrapper for a few lines that implement a clustering
#' analysis. Given an input dataframe and number of clusters, this will conduct
#' hierarchical cluster using choices that mimic those in Winters et al and
#' others. Clustering is performed on all variables in the input dataframe;
#' often specific variables are chosen at the outset (e.g. x and y).
#'
#' @param df dataframe to cluster; should contain only variables that are used
#' in the clustering process
#' @param k number of clusters to identify
#'
#' @return vector of cluster assignments
#' @importFrom magrittr "%>%"
#' @import dplyr
#' @import tidyr
#' @import modelr
#' @export
#'
#' @examples
#'winters_df = real_dfs %>%
#'filter(name == "Winters", x != 66) %>%
#'mutate(
#'ii = 66 - x,
#'delta = y  - x,
#'prop = delta / (66 - x))
#'hclust_fun(winters_df %>% select(x, y), 2)
hclust_fun = function(df, k) {

  dist_mat = ecodist::distance(df, method = "mahalanobis")
  cluster_fit = hclust(dist_mat, method = "ward.D2")
  list(cluster = cutree(cluster_fit, k = k))

}

