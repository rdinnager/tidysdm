#' Get Spatial Neighbours
#'
#' @param x An `sf` object
#' @param k The number of nearest neighbours to retrieve
#' @param maxdist The maximum distance to get nearest neighbours from
#' @param progress Print progess bar?
#' @param parallel How many parallel cores to use.
#'
#' @return A list of neighbour indices
#' @export
get_spatial_neighbours <- function(x, k = 8, maxdist = 100000, progress = TRUE, parallel = 1) {
  x <- sf::st_sf(x)
  nngeo::st_nn(x, x,
               k = k,
               maxdist = maxdist,
               progress = progress,
               parallel = parallel)

}
