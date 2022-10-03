step_sample_pseudo_absences <- function(
  recipe,
  bg_geometry = NULL,
  role = "predictor",
  trained = FALSE,
  size,
  options = list(type = "random", exact = FALSE),
  skip = FALSE,
  id = rand_id("sample_pseudo_absences")
  ) {

  add_step(
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
    mutate(abs_pnts = new_col)

  new_data

}

sample_geom <- function(x, size, args) {
  res <- rlang::exec(sf::st_sample, x = x, size = size, !!!args)
  res
}
