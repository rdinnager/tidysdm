bg_convert <- function(bg) {
  if(!inherits(bg, "stars")) {
    bg <- stars::st_as_stars(bg)
  }
  bg_sf <- sf::st_as_sf(bg,
                        na.rm = TRUE,
                        merge = TRUE)
  bg_sf
}
