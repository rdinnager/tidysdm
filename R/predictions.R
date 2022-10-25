#' Generate a grid of values for making predictions
#'
#' @param bg
#' @param n
#' @param square
#' @param include_polygons
#'
#' @return
#' @export
#'
#' @examples
create_prediction_grid <- function(bg, n = 10000,
                                   square = TRUE,
                                   include_polygons = FALSE) {

  bg <- bg_convert(bg)

  type <- ifelse(square, "regular", "hexagonal")

  grid <- sdm_data(NULL, bg = bg, n = n, sample_options = list(type = "regular"))

  if(include_polygons) {
    grid_xy <- sf::st_coordinates(grid) %>%
      as.data.frame() %>%
      purrr::map_int(~ dplyr::n_distinct(.x))

    grid <- sf::st_make_grid(bg, n = grid_xy, square = square, what = "centers") %>%
      sf::st_sf() %>%
      mutate(polygon = sf::st_make_grid(bg, n = grid_xy, square = square, what = "polygons")) %>%
      sf::st_intersection(sf::st_geometry(bg)) %>%
      rename_geom("pnts") %>%
      dplyr::mutate(present = "absent",
                    pnt_origin = "prediction_grid",
                    pnts = sf::st_cast(pnts, "GEOMETRY"))

  }

  grid

}
