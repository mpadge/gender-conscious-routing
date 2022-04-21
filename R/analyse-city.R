#' gcr_city
#'
#' Analyse entire properties of one city
#'
#' @param net Street network in `osmdata_sc` format.
#' @param wt_profile Type of weighting profile for street network routing,.
#' @param n Number of pints used to calculate pair-wise routes along which to
#' aggregate statistics. If no value specified, routes are calculated between
#' all pairs for the entire network.
#' @export
gcr_city <- function (net, wt_profile = "foot", n = NULL) {
}
