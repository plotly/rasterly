#' @export
print.rasterly <- function(x, ...) {
  
  if(!is.rasterlyBuild(x)) x <- rasterly_build(x)
  
  grid::grid.newpage()
  
  if(is.null(x$image)) {
    message("No image was found.\n 
            Maybe you forget to pipe layers or set `show_image = FALSE`?")
    invisible(grid::grid.draw(grid::grob()))
  } else {
    invisible(grid::grid.raster(x$image))
  }
}
