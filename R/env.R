#' Add environmental variables
#'
#' @param x
#' @param env
#'
#' @return
#' @export
#'
#' @examples
add_env_vars <- function(x, env) {
  x <- sf::st_sf(x)

  env <- check_env(env)

  env_vars <- stars::st_extract(env, sf::st_coordinates(x))
  colnames(env_vars) <- stars::st_get_dimension_values(env, "band")
  x <- x %>%
    dplyr::bind_cols(env_vars)

  x
}
