#' @export
print.rasterizer <- function(rastObj) {
  
  if(is.rasterizeLayer(rastObj) && !is.rasterizer_build(rastObj)) rastObj <- rasterizer_build(rastObj)
  
  grid::grid.newpage()
  
  if(is.null(rastObj$image)) {
    message("No image is found")
    grid::grid.draw(grid::grob())
  } else {
    grid::grid.raster(rastObj$image)
  }
  invisible()
}