#' @title Is \code{rasterly}
#' @description Reports whether x is a \code{rasterly} object.
#' @param x a \code{rasterly} object
#' @export
is.rasterly <- function(x) {
  inherits(x, "rasterly")
}

#' @title Is \code{rasterlyBuild}
#' @description Reports whether x is a \code{rasterlyBuild} object. In other word, it helps to define
#' whether this object has been passed through `rasterly_build`
#' @param x a \code{rasterly} object
#' @export
is.rasterlyBuild <- function(x) {
  inherits(x, "rasterlyBuild")
}

is.rasterlyLayer <- function(x) {
  inherits(x, "rasterlyLayer")
}
