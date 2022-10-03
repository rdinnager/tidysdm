step_sample_pseudo_absences <- function(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  size,
  options = list(type = "random", exact = FALSE),
  skip = FALSE,
  id = rand_id("sample_pseudo_absences")
  ) {

  terms <- ellipse_check(...)

  add_step(
    recipe,
    step_sample_pseudo_absences_new(
      terms = terms,
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
  function(terms, role, trained, size, options, skip, id) {
    step(
      subclass = "sample_pseudo_absences",
      terms = terms,
      role = role,
      trained = trained,
      size = size,
      options = options,
      skip = skip,
      id = id
    )
  }

prep.step_sample_pseudo_absences <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  ## You can add error trapping for non-numeric data here and so on.
  if(length(col_names) == 0) {
    col_names <- attr(training, "sf_column")
  }
  if(any(purrr::map_lgl(training[, col_names], ~!inherits(.x, "sfc")))) {
   rlang::abort("step_sample_pseudo_absences only works with sfc geoemtry columns.")
  }

  ## Use the constructor function to return the updated object.
  ## Note that `trained` is now set to TRUE

  step_percentile_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    ref_dist = ref_dist,
    options = x$options,
    skip = x$skip,
    id = x$id
  )
}
