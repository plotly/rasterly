#' @title Extract or replace parts of a `rasterly` object
#' @description The `extract` function provides functionality for updating existing `rasterly` objects.
#' @param x Object from which to extract element(s) or in which to replace element(s).
#' @param name Character. A literal string to be extracted from `x`. See details for more information.
#'
#' @details Available names:
#' \itemize{
#'  \item{Aggregation: }{"data", "mapping", "plot_width", "plot_height", "range", "x_range",
#' "y_range", "xlim", "ylim", "aesthetics", "reduction_func", "glyph",
#' "max_size", "group_by_data_table", "drop_data", "variable_check"}
#'  \item{Display: }{"background", "colour_map", "colour_key", "alpha", "span",
#'  "show_raster", "layout"}
#' }
#'
#' @usage
#' x[name]
#'
#' x[name] <- value
#'
#' @examples
#' library(rasterly)
#' r <- rasterly(
#'        data = data.frame(x = 1:1e4, y = runif(1e4), category = sample(1:4, 1e4, replace = TRUE)),
#'        mapping = aes(x = x, y = y)
#' ) %>%
#'   rasterize_points(xlim = c(1, 5000)) %>%
#'   rasterize_points(
#'     mapping = aes(x = x, y = y, colour = category),
#'     xlim = c(5001, 1e4)
#'   )
#' r["mapping"]
#' r["xlim"]
#'
#' # reassign parent `rasterly()` mapping
#' r["mapping"] <- aes(x = x, y = y, colour = category)
#' r["mapping"]
#'
#' # reassign all mapping systems
#' r["mapping", which = 1:length(r)] <- aes(x = x, y = y)
#' r["mapping"]
#' @export
`[.rasterly` <- function(x, name) {
  # x is executed
  if(is.rasterly_build(x)) {
    .Primitive("[")(x,name)
  } else {
    # x is an unexecuted list of environments
    l <- lapply(x,
                function(envir) {
                  .get(name, envir = envir)
                })
    return(l)
  }
}

#' @inherit [.rasterly
#' @param level Numeric. Specifies level of `rasterly` object to modify; default is 1 for the parent layer (`rasterly()`).
#' @usage
#' x[name]
#'
#' x[name] <- value
#' @export
`[<-.rasterly` <- function(x, name, value, level = 1) {

  # x is executed
  if(is.rasterly_build(x)) {
    .Primitive("[<-")(x, name, value)
  } else {
    # x is an unexecuted list of environments
    for(l in level) {
      assign(name, value, envir = x[[l]])
    }
    invisible(return(x))
  }
}
