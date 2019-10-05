#' @export
print.rasterly <- function(x, ...) {
  
  if(is.rasterizeLayer(x) && !is.rasterly_build(x)) x <- rasterly_build(x)
  
  grid::grid.newpage()
  
  if(is.null(x$image)) {
    message("No image is found")
    invisible(grid::grid.draw(grid::grob()))
  } else {
    invisible(grid::grid.raster(x$image))
  }
}