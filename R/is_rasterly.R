#' @title Is \code{rasterly}
#' @description Reports whether x is a \code{rasterly} object.
#' @param x a \code{rasterly} object
#' @export
is.rasterly <- function(x) {
  inherits(x, "rasterly")
}

is.rasterly_build <- function(x) {
  inherits(x, "rasterly_build")
}

is.rasterizeLayer <- function(x) {
  inherits(x, "rasterlyLayer")
}