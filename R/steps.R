#' Title
#'
#' @param recipe
#' @param bg_geometry
#' @param role
#' @param trained
#' @param size
#' @param options
#' @param skip
#' @param id
#'
#' @return
#' @export
#'
#' @examples
step_sample_pseudo_absences <- function(
  recipe,
  bg_geometry = NULL,
  role = NA,
  trained = FALSE,
  size,
  options = list(type = "random", exact = FALSE),
  skip = FALSE,
  id = rand_id("sample_pseudo_absences")
  ) {

  recipe <- add_step(
    recipe,
    step_sample_pseudo_absences_new(
      bg_geometry = rlang::enquos(bg_geometry),
      trained = trained,
      role = role,
      size = size,
      options = options,
      skip = skip,
      id = id
    )
  )

  recipe
}

step_sample_pseudo_absences_new <-
  function(bg_geometry, role, trained, size, options, skip, id) {
    step(
      subclass = "sample_pseudo_absences",
      bg_geometry = bg_geometry,
      role = role,
      trained = trained,
      size = size,
      options = options,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_sample_pseudo_absences <- function(x, training, info = NULL, ...) {

  bg_geom <- recipes_eval_select(x$bg_geometry, training, info)

  if(length(bg_geom) == 0) {
    bg_geom <- attr(training, "sf_column")
  }
  if(!inherits(training[, bg_geom, drop = TRUE], "sfc")) {
   rlang::abort("bg_geometry should be an sfc geometry column.")
  }

  ## Use the constructor function to return the updated object.

  step_sample_pseudo_absences_new(
    bg_geometry = bg_geom,
    trained = TRUE,
    role = x$role,
    size = x$size,
    options = x$options,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_sample_pseudo_absences <- function(object, new_data, ...) {

  check_new_data(object$bg_geometry, object, new_data)

  geom <- new_data[ , object$bg_geometry, drop = TRUE]
  num_poly <- length(geom)

  size <- max(1, round(object$size / num_poly, 0))

  new_col <- geom %>%
    purrr::map(~ sf::st_combine(sample_geom(.x, size = size, args = object$options)))

  new_col <- do.call(c, new_col)

  new_data <- new_data %>%
    mutate(absent = new_col)

  new_data <- expand_pres_abs(new_data)

  new_data

}

sample_geom <- function(x, size, args) {
  res <- rlang::exec(sf::st_sample, x = x, size = size, !!!args)
  res
}


step_expand_pres_abs <- function(
  recipe,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = rand_id("expand_pres_abs")
  ) {

  add_step(
    recipe,
    step_expand_pres_abs_new(
      trained = trained,
      role = role,
      skip = skip,
      id = id
    )
  )
}

step_expand_pres_abs_new <-
  function(role, trained, skip, id) {
    step(
      subclass = "expand_pres_abs",
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_expand_pres_abs <- function(x, training, info = NULL, ...) {

  ## Use the constructor function to return the updated object.

  step_expand_pres_abs_new(
    trained = TRUE,
    role = x$role,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_expand_pres_abs <- function(object, new_data, ...) {

  check_new_data("pres_pnts", object, new_data)

  expand_pres_abs(new_data)

}

expand_pres_abs <- function(new_data) {

  pres <- sf::st_sf(new_data %>%
                      dplyr::select(id, .data$present) %>%
                      dplyr::filter(!sf::st_is_empty(.data$present))) %>%
    sf::st_cast("POINT", warn = FALSE) %>%
    rename(geom = .data$present) %>%
    dplyr::mutate(present = 1)

  if("absent" %in% colnames(new_data)) {

  abs <- sf::st_sf(new_data %>%
                      dplyr::select(id, .data$absent) %>%
                     dplyr::filter(!sf::st_is_empty(.data$absent))) %>%
    sf::st_cast("POINT", warn = FALSE) %>%
    rename(geom = .data$absent) %>%
    dplyr::mutate(present = 0)

  } else {

    abs <- dplyr::tibble()

  }

  dplyr::bind_rows(pres,
                   abs) %>%
    mutate(present = as.integer(.data$present))

}
