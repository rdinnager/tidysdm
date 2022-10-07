#' @export
recipe.sdm_data <- function(x, ...) {
  NextMethod("recipe", x,
             vars = c("present", "pnts", "pnt_origin"),
             roles = c("outcome", "coordinates", "info"))

}
