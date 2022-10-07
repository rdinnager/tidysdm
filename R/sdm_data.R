#' Convert presence points and a background area
#' into an `sdm_data` object suitable for a
#' `tidysdm` workflow.
#'
#' @param pres Points representing species
#' occurrences. Should be an `sf` object with `crs`
#' information
#' @param bg A background area as an `sf` polygon or
#' a raster mask (in `stars` or `raster` format)
#' @param n Integer specifying the number of background or pseudo-absence
#' points to sample from the background as specified by `bg`.
#' @param abs Optionally an `sf` object with true absence data in the form of
#' points.
#' @param ...
#'
#' @return A `sdm_data` object inheriting from a `tibble`
#' @export
#'
#' @examples
sdm_data <- function(pres, bg, n = 500, abs = NULL, sample_options = list(), ...) {
  if(!inherits(pres, "sf")) {
    pres <- sf::st_as_sf(pres, ...)
  }

  bg <- bg_convert(bg)

  # bbox <- sf::st_bbox(bg)
  # w <- bbox[3] - bbox[1]
  # h <- bbox[4] - bbox[2]
  #
  # y <- sqrt(n *(h / w))
  # x <- (w/h) * y
  #
  # n <- c(ceiling(x), ceiling(y))
  #
  # bg_grid <- sf::st_make_grid(bg, n = n,
  #                             square = FALSE) %>%
  #   sf::st_sf() %>%
  #   sf::st_join(bg %>% mutate(pres = 1)) %>%
  #   filter(!is.na(pres)) %>%
  #   dplyr::mutate(id = seq_len(dplyr::n())) %>%
  #   dplyr::select(id) %>%
  #   sf::st_make_valid()

  if(sf::st_crs(bg) != sf::st_crs(pres)) {
    rlang::warn("bg and presence_pnts projections (crs) do not match! Attempting to transform bg to presence_pnts' crs")
    bg <- sf::st_transform(bg, sf::st_crs(pres))
  }

  abs2 <- rlang::exec(sf::st_sample, x = bg, size = n, !!!sample_options) %>%
    sf::st_as_sf() %>%
    dplyr::mutate(present = 0, pnt_origin = "pseudo")

  abs2 <- rename_geom(abs2, "pnts")

  if(is.null(abs)) {
    abs <- abs2
  } else {
    abs <- abs %>%
      rename_geom("pnts") %>%
      dplyr::mutate(present = 1, pnt_origin = "data") %>%
      bind_rows(abs2)
  }

  sdm_dat <- pres %>%
    rename_geom("pnts") %>%
    dplyr::mutate(present = 1, pnt_origin = "data") %>%
    dplyr::bind_rows(abs)

  class(sdm_dat) <- c("sdm_data", class(sdm_dat))

  sdm_dat %>%
    dplyr::select(.data$pnts, .data$present, .data$pnt_origin) %>%
    dplyr::mutate(present = as.integer(.data$present))

}
