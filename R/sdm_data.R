#' Convert presence points and a background area
#' into an `sdm_data` object suitable for a
#' `tidysdm` workflow.
#'
#' @param presence_pnts Points representing species
#' occurrences. Should be an `sf` object with `crs`
#' information
#' @param bg A background area as an `sf` polygon or
#' a raster mask (in `stars` or `raster` format)
#' @param n Number of grid cells to divide the background
#' area into. These grid cells will be used for spatial
#' resampling (e.g. with `spatialsample` package) or
#' thinning (using `step_thin()`)
#' @param ...
#'
#' @return A `sdm_data` object inheriting from a `tibble`
#' @export
#'
#' @examples
sdm_data <- function(presence_pnts, bg, n = 500, ...) {
  if(!inherits(presence_pnts, "sf")) {
    presence_pnts <- sf::st_as_sf(presence_pnts, ...)
  }
  bg <- bg_convert(bg)

  bbox <- sf::st_bbox(bg)
  w <- bbox[3] - bbox[1]
  h <- bbox[4] - bbox[2]

  y <- sqrt(n *(h / w))
  x <- (w/h) * y

  n <- c(ceiling(x), ceiling(y))

  bg_grid <- sf::st_make_grid(bg, n = n,
                              square = FALSE) %>%
    sf::st_sf() %>%
    sf::st_join(bg %>% mutate(pres = 1)) %>%
    filter(!is.na(pres)) %>%
    dplyr::mutate(id = seq_len(dplyr::n())) %>%
    dplyr::select(id) %>%
    sf::st_make_valid()

  if(sf::st_crs(bg_grid) != sf::st_crs(presence_pnts)) {
    rlang::warn("bg and presence_pnts projections (crs) do not match! Attempting to transform bg to presence_pnts' crs")
    bg_grid <- sf::st_transform(bg_grid, sf::st_crs(presence_pnts))
  }

  pp <- presence_pnts %>%
    sf::st_join(bg_grid) %>%
    dplyr::group_by(.data$id) %>%
    dplyr::summarise(present = sf::st_combine(.data$geometry))

  sdm_dat <- bg_grid %>%
    dplyr::left_join(as_tibble(pp), by = "id")

  g <- attr(sdm_dat, "sf_column")

  colnames(sdm_dat)[colnames(sdm_dat) == g] <- "geom"
  sf::st_geometry(sdm_dat) <- "geom"

  class(sdm_dat) <- c("sdm_data", class(sdm_dat))

  sdm_dat

}
