#' @title Is \code{rasterly}
#' @description Reports whether x is a \code{rasterly} object.
#' @param x a \code{rasterly} object
#' @export
is.rasterly <- function(x) {
  inherits(x, "rasterly")
}

is.rasterlyBuild <- function(x) {
  inherits(x, "rasterlyBuild")
}

is.rasterlyLayer <- function(x) {
  inherits(x, "rasterlyLayer")
}
