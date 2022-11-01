#' Spatial Cross Validation Designed for Presence-Only Data
#'
#' @param data
#' @param radius
#' @param buffer
#' @param v
#' @param repeats
#' @param presence
#' @param pool
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
po_spatial_buffer_vfold_cv <- function(data,
                                       radius = NULL,
                                       buffer = 0.1,
                                       v = 10,
                                       repeats = 1,
                                       presence = NULL,
                                       pool = 0.1,
                                       ...) {

  grid <- sf::st_make_grid(data, ...)
  #grid <- sf::st_make_grid(data, n = c(50, 50))

  grid_intersect <- sf::st_intersects(grid, data)

  pres_lev <- levels(data[ , presence, drop = TRUE])
  if(length(pres_lev) != 2) {
    rlang::abort("presence variable must have exactly two levels (and the second level should represent presence)")
  }

  grid_df <- sf::st_sf(geometry = grid) %>%
    dplyr::mutate("id" := grid_intersect) %>%
    dplyr::rowwise() %>%
    dplyr::filter(length(.data$id) > 0) %>%
    dplyr::mutate("presence" := any(data[ , {presence}, drop = TRUE][id] == "present")) %>%
    dplyr::mutate("presence" := as.character(.data$presence)) %>%
    dplyr::ungroup()

  grid_cv <- spatialsample::spatial_buffer_vfold_cv(grid_df,
                                                    radius = radius,
                                                    buffer = buffer,
                                                    v = v,
                                                    repeats = repeats,
                                                    strata = "presence",
                                                    pool = pool)


  grid_cv$splits <- purrr::map(grid_cv$splits,
                               ~ expand_gridded_rsplit(.x, data))


  grid_cv

}

expand_gridded_rsplit <- function(object, orig_dat) {
  object$data$in_out <- NA
  object$data$in_out[object$in_id] <- "in"
  object$data$in_out[object$out_id] <- "out"

  old_class <- class(orig_dat)

  new_data <- object$data %>%
    dplyr::as_tibble() %>%
    dplyr::select(dplyr::all_of(c("id", "in_out"))) %>%
    dplyr::rowwise() %>%
    dplyr::summarise(df = list(orig_dat %>% dplyr::slice(id)),
                     in_out = in_out[[1]]) %>%
    tidyr::unnest(dplyr::all_of("df"))

  in_id <- which(new_data$in_out == "in")
  out_id <- which(new_data$in_out == "out")

  object$in_id <- in_id
  object$out_id <- out_id

  new_data <- new_data %>%
    dplyr::select(-dplyr::all_of("in_out")) %>%
    sf::st_sf()
  class(new_data) <- old_class

  object$data <- new_data

  object

}


