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

bake.step_sample_pseudo_absences <- function(object, new_data, ...) {

  #check_new_data(object$bg_geometry, object, new_data)

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

prep.step_expand_pres_abs <- function(x, training, info = NULL, ...) {

  ## Use the constructor function to return the updated object.

  step_expand_pres_abs_new(
    trained = TRUE,
    role = x$role,
    skip = x$skip,
    id = x$id
  )
}

bake.step_expand_pres_abs <- function(object, new_data, ...) {

  #check_new_data("pres_pnts", object, new_data)

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

#' @export
step_thin_pseudo_absences <- function(
  recipe,
  pres = NULL,
  pnts = NULL,
  abs2pres_ratio = 10,
  role = NA,
  trained = FALSE,
  skip = FALSE,
  id = rand_id("thin_pseudo_absences")
  ) {

  add_step(
    recipe,
    step_thin_pseudo_absences_new(
      pres = rlang::enquos(pres),
      pnts = rlang::enquos(pnts),
      abs2pres_ratio = abs2pres_ratio,
      trained = trained,
      role = role,
      skip = skip,
      id = id
    )
  )
}

step_thin_pseudo_absences_new <-
  function(pres, pnts, abs2pres_ratio, role, trained, skip, id) {
    step(
      subclass = "thin_pseudo_absences",
      pres = pres,
      pnts = pnts,
      abs2pres_ratio = abs2pres_ratio,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_thin_pseudo_absences <- function(x, training, info = NULL, ...) {

  pres <- recipes_eval_select(x$pres, training, info)
  pnts <- recipes_eval_select(x$pnts, training, info)

  abs2pres_ratio <- as.integer(x$abs2pres_ratio)

  if(length(pres) == 0) {
    pres <- "present"
  }

  if(length(pnts) == 0) {
    pnts <- "pnts"
  }

  ## Use the constructor function to return the updated object.

  step_thin_pseudo_absences_new(
    pres = pres,
    pnts = pnts,
    abs2pres_ratio = abs2pres_ratio,
    trained = TRUE,
    role = x$role,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_thin_pseudo_absences <- function(object, new_data, ...) {

  #check_new_data(c(object$pres, object$pnts), object, new_data)

  presents <- new_data[ , object$pres, drop = TRUE] == "present"

  num_pres <- sum(presents)
  num_abs <- round(object$abs2pres_ratio * num_pres, 0)

  new_data <- new_data %>%
    dplyr::filter(!presents) %>%
    dplyr::slice_sample(n = num_abs) %>%
    dplyr::bind_rows(new_data %>%
                       dplyr::filter(presents)) %>%
    sf::st_sf(sf_column_name = object$pnts)

  new_data

}

#' @export
print.step_thin_pseudo_absences <-
  function(x, width = max(20, options()$width - 39), ...) {
    title <- paste("Thinning pseudo-absences to 1 per every",
                   x$abs2pres_ratio,
                   "presence points\n")
    cat(title)
    if (x$trained) {
      cat(" [trained]\n")
    }
    invisible(x)
  }


################ working here ####

#' @export
step_add_env_vars <- function(
  recipe,
  pnts = NULL,
  env,
  role = "predictor",
  trained = FALSE,
  skip = FALSE,
  id = rand_id("add_env_vars")
  ) {

  add_step(
    recipe,
    step_add_env_vars_new(
      pnts = rlang::enquos(pnts),
      env = env,
      trained = trained,
      role = role,
      skip = skip,
      id = id
    )
  )
}

step_add_env_vars_new <-
  function(pnts, env, role, trained, skip, id) {
    step(
      subclass = "add_env_vars",
      pnts = pnts,
      env = env,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_add_env_vars <- function(x, training, info = NULL, ...) {

  pnts <- recipes_eval_select(x$pnts, training, info)

  if(length(pnts) == 0) {
    pnts <- attr(training, "sf_column")
  }

  ## Use the constructor function to return the updated object.

  step_add_env_vars_new(
    pnts = pnts,
    env = x$env,
    trained = TRUE,
    role = x$role,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_add_env_vars <- function(object, new_data, ...) {

  #check_new_data(object$pnts, object, new_data)

  # new_data <- sf::st_sf(new_data)
  #
  # env <- check_env(object$env)
  #
  # env_vars <- stars::st_extract(env, sf::st_coordinates(new_data))
  # colnames(env_vars) <- stars::st_get_dimension_values(env, "band")
  # new_data <- new_data %>%
  #   dplyr::bind_cols(env_vars) %>%
  #   sf::st_sf(sf_column_name = object$pnts)

  new_data <- add_env_vars(new_data, object$env)

  dplyr::as_tibble(new_data)

}

#' @export
print.step_add_env_vars <-
  function(x, width = max(20, options()$width - 39), ...) {
    title <- paste("Add environmental variables. ")
    cat(title)
    if (x$trained) {
      cat(" [trained]\n")
    }
    invisible(x)
  }

check_env <- function(env) {
  if(!inherits(env, "stars")) {
    env <- stars::st_as_stars(env)
  }
  env
}


################ spatial lag ##################################

#' @export
step_initialize_spatial_lags <- function(
  recipe,
  col_names = "present",
  role = "predictor",
  trained = FALSE,
  k = 8,
  skip = FALSE,
  id = rand_id("add_env_vars")
  ) {

  #terms <- ellipse_check(...)

  add_step(
    recipe,
    step_initialize_spatial_lags_new(
      trained = trained,
      role = role,
      k = k,
      col_names = col_names,
      skip = skip,
      id = id
    )
  )
}

step_initialize_spatial_lags_new <-
  function(role, trained, k, col_names, skip, id) {
    step(
      subclass = "initialize_spatial_lags",
      role = role,
      trained = trained,
      k = k,
      col_names = col_names,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_initialize_spatial_lags <- function(x, training, info = NULL, ...) {

  #col_names <- recipes_eval_select(x$terms, training, info)

  ## Use the constructor function to return the updated object.

  step_initialize_spatial_lags_new(
    trained = TRUE,
    role = x$role,
    k = x$k,
    col_names = x$col_names,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_initialize_spatial_lags <- function(object, new_data, ...) {

  #check_new_data(object$col_names, object, new_data)

  new_names <- paste0(object$col_names,
                          "_spat_",
                          object$k)

  temp <- as.data.frame(matrix(0, nrow = nrow(new_data), ncol = length(object$col_names)))

  colnames(temp) <- new_names

  dplyr::bind_cols(new_data, temp)

}

#' @export
print.step_initialize_spatial_lags <-
  function(x, width = max(20, options()$width - 39), ...) {
    cat("Initialize spatial lag variables on ", sep = "")
    printer(
      # Names before prep (could be selectors)
      #untr_obj = rlang::ensyms(x$col_names),
      # Names after prep:
      tr_obj = x$col_names,
      # Has it been prepped?
      trained = x$trained,
      # An estimate of how many characters to print on a line:
      width = width
    )
    invisible(x)
  }



#' @export
step_fill_spatial_lags <- function(
  recipe,
  ...,
  pnts = NULL,
  neighbours = NULL,
  role = "predictor",
  trained = FALSE,
  k = 8,
  maxdist = 500000,
  parallel = 1,
  col_names = NULL,
  skip = TRUE,
  id = rand_id("add_env_vars")
  ) {

  terms <- ellipse_check(...)

  add_step(
    recipe,
    step_fill_spatial_lags_new(
      terms = terms,
      pnts = rlang::enquos(pnts),
      neighbours = rlang::enquos(neighbours),
      trained = trained,
      role = role,
      k = k,
      maxdist = maxdist,
      parallel = parallel,
      col_names = col_names,
      skip = skip,
      id = id
    )
  )
}

step_fill_spatial_lags_new <-
  function(terms, pnts, neighbours, role, trained, k, maxdist, parallel, col_names, skip, id) {
    step(
      subclass = "fill_spatial_lags",
      terms = terms,
      pnts = pnts,
      neighbours = neighbours,
      role = role,
      trained = trained,
      k = k,
      maxdist = maxdist,
      parallel = parallel,
      col_names = col_names,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_fill_spatial_lags <- function(x, training, info = NULL, ...) {

  pnts <- recipes_eval_select(x$pnts, training, info)
  neighbours <- recipes_eval_select(x$neighbours, training, info)
  col_names <- recipes_eval_select(x$terms, training, info)

  if(length(pnts) == 0) {
    pnts <- attr(training, "sf_column")
  }

  if(length(neighbours) == 0) {
    dat <- training[ , pnts]
    nn <- get_spatial_neighbours(dat, k = x$k, maxdist = x$maxdist, parallel = x$parallel)
  } else {
    nn <- neighbours
  }

  step_fill_spatial_lags_new(
    terms = x$terms,
    pnts = pnts,
    neighbours = nn,
    trained = TRUE,
    role = x$role,
    k = x$k,
    maxdist = x$maxdist,
    parallel = x$parallel,
    col_names = col_names,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_fill_spatial_lags <- function(object, new_data, ...) {

  check_new_data(object$col_names, object, new_data)
  #check_new_data(object$pnts, object, new_data)
  #check_new_data(object$neighbours, object, new_data)

  new_data <- dplyr::as_tibble(new_data)

  temp <- new_data[, object$col_names] %>%
    dplyr::mutate(neighbours = object$neighbours) %>%
    dplyr::mutate(dplyr::across(.cols = dplyr::all_of(object$col_names),
                                .fns = function(x) purrr::map_dbl(neighbours,
                                                                  function(y) mean(as.numeric(x[y]) - 1,
                                                                         na.rm = TRUE)))) %>%
    dplyr::mutate(dplyr::across(.cols = dplyr::all_of(object$col_names),
                                .fns = ~ (.x - mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE))) %>%
    dplyr::select(-neighbours)

  new_names <- paste0(object$col_names,
                      "_spat_",
                      object$k)
  colnames(temp) <- new_names

  new_data[ , new_names] <- NULL

  dplyr::bind_cols(new_data, temp)

}

#' @export
print.step_fill_spatial_lags <-
  function(x, width = max(20, options()$width - 39), ...) {
    cat("Fill spatial lag variables on ", sep = "")
    printer(
      # Names before prep (could be selectors)
      untr_obj = x$terms,
      # Names after prep:
      tr_obj = x$col_names,
      # Has it been prepped?
      trained = x$trained,
      # An estimate of how many characters to print on a line:
      width = width
    )
    invisible(x)
  }
