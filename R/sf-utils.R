bg_convert <- function(bg) {
  if(inherits(bg, "sf")) {
    return(bg)
  }
  if(inherits(bg, "sfc")) {
    return(bg)
  }
  if(!inherits(bg, "stars")) {
    bg <- stars::st_as_stars(bg)
  }
  bg_sf <- sf::st_as_sf(bg,
                        na.rm = TRUE,
                        merge = TRUE)
  bg_sf
}

rename_geom <- function(x, new_geom_name) {
  g <- attr(x, "sf_column")
  colnames(x)[colnames(x) == g] <- new_geom_name
  sf::st_geometry(x) <- new_geom_name
  x
}
