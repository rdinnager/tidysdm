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
create_background <- function(x, method = c("convex_hull", "point_buffer", "ecoregion", "concave_hull", "grid_fill", "user_fill"), buffer = 0, max_bg = NULL, grid_options = NULL, concave_options = NULL) {
  method <- match.arg(method)
  bg <- switch(method,
         convex_hull = sf::st_convex_hull(sf::st_union(x)) %>%
           sf::st_buffer(buffer),
         point_buffer = sf::st_buffer(x, buffer) %>%
           sf::st_union(),
         ecoregion = extract_ecoregion(x, buffer),
         stop("method is not currently supported."))
  if(!is.null(max_bg)) {
    bg <- sf::st_intersection(bg, max_bg)
  }
  bg
}

extract_ecoregion <- function(x, buffer) {
  ecoregions <- dplyr::bind_rows(Afrotropic, Australasia,
                                 Indomalayan, Nearctic,
                                 Neotropic, Oceania,
                                 Palearctic) %>%
    sf::st_make_valid() %>%
    dplyr::filter(sf::st_is_valid(geometry))

  eco_which <- map_lgl(sf::st_intersects(ecoregions, x), ~length(.x) > 0)

  sf::st_union(ecoregions[eco_which, ])

}

