#' @export
print.rasterly <- function(rastObj) {
  
  if(is.rasterizeLayer(rastObj) && !is.rasterly_build(rastObj)) rastObj <- rasterly_build(rastObj)
  
  grid::grid.newpage()
  
  if(is.null(rastObj$image)) {
    message("No image is found")
    invisible(grid::grid.draw(grid::grob()))
  } else {
    invisible(grid::grid.raster(rastObj$image))
  }
}