#' @export
recipe.sdm_data <- function(x, ...) {
  NextMethod("recipe", x,
             vars = c("id", "geom", "present"),
             roles = c("id", "geom", "outcome"))
  # recipe(x, vars = c("id", "geom", "present"),
  #        roles = c("id", "geom", "outcome"))
}
