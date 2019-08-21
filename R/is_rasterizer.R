#' @export
is.canvas <- function(x) {
  inherits(x, "canvas")
}

#' @export
is.rasterizer <- function(x) {
  inherits(x, "rasterizer")
}

#' @export
is.aggregation <- function(x) {
  inherits(x, "aggregation")
}