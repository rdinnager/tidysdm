#' Make background from points
#'
#' @param x
#' @param method
#' @param buffer
#' @param grid_options
#' @param concave_options
#'
#' @return
#' @export
#'
#' @examples
create_background <- function(x, method = c("convex_hull", "point_buffer", "ecoregion", "concave_hull", "grid_fill", "user_fill"), buffer = 1, max_bg = NULL, grid_options = NULL, concave_options = NULL) {
  method <- match.arg(method)
  bg <- switch(method,
         convex_hull = sf::st_convex_hull(sf::st_union(x)) %>%
           sf::st_buffer(buffer),
         point_buffer = sf::st_buffer(x, buffer) %>%
           sf::st_union(),
         stop("method is not currently supported."))
  if(!is.null(max_bg)) {
    bg <- sf::st_intersection(bg, max_bg)
  }
  bg
}
