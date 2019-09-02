#' @export
print.rasterizer <- function(rastObj) {
  
  if(is.rasterizeLayer(rastObj) && !is.rasterizer_build(rastObj)) rastObj <- rasterizer_build(rastObj)
  grid.rasterizer(rastObj)
  invisible()
}