#' @title `Rasterizer` to `plotly`
#' @description Display raster image via `plotly`
#' @param rastObj A rasterizer object
#' @param as_heatmap Draw `plotly` by adding heatmap layer. See \code{add_rasterizer}
#' @param scaling Scale layout matrix. 
#' @param ... Arguments to the layout object. For documentation, 
#' see https://plot.ly/r/reference/#Layout_and_layout_style_objects
#' @export
#' 
#' @examples 
#' \dontrun{
#'    library(rasterizer)
#'    if(requireNamespace("plotly")) {
#'      x <- rnorm(1e7)
#'      y <- rnorm(1e7)
#'      category <- sample(1:5, 1e7, replace = TRUE)
#'      data.frame(x = x, y = y, category = category) %>%
#'        canvas(mapping = aes(x = x, y = y)) %>%
#'        aggregation_points(layout = "weighted") %>%
#'        rasterizer() %>% 
#'        plotly.rasterizer()
#'    }
#' }
plotly.rasterizer <- function(rastObj, as_heatmap = FALSE, 
                              scaling = c("log", "to01", "origin"), 
                              ...) {
  
  if(missing(rastObj) || !is.rasterizer(rastObj)) stop("No 'rasterizer' object", call. = FALSE)
  scaling <- match.arg(scaling)
  
  if(as_heatmap) {
    if(sum(lengths(rastObj$agg)) > 1) 
      message("More than one aggregation matrix is detected. 
            Set `as_heatmap = FALSE` is recommended.")
    z <- rastObj$agg[[1]][[1]]
    dimZ <- dim(z)
    y <- seq(rastObj$y_range[1], rastObj$y_range[2], length.out = dimZ[1])
    x <- seq(rastObj$x_range[1], rastObj$x_range[2], length.out = dimZ[2])
    
    switch(scaling, 
           "log" = {
             z <- matrix(log(z + 1), nrow = dimZ[1])
           }, 
           "to01" = {
             z <- zeroOneTransform(z)
           }, 
           "origin" = NULL)
    
    p <- plotly::plot_ly(x = x, y = y) %>% 
      plotly::add_heatmap(z = z)
  } else {
    image <- rastObj$image
    if(is.null(image)) 
      stop("No image is found. Consider set `show_raster = TRUE` in `canvas()`?", call. = FALSE)
    
    var_names <- unlist(rastObj$variable_names)
    
    p <- plotly::plot_ly(
      width = rastObj$plot_width,
      height = rastObj$plot_height
    ) %>%
      plotly::layout(
        images = list(
          source = plotly::raster2uri(image), # converts a raster object to a data URI.
          xref = "x", 
          yref = "y", 
          x = rastObj$x_range[1], 
          y = rastObj$y_range[1], 
          sizex = diff(rastObj$x_range), 
          sizey = diff(rastObj$y_range),
          xanchor = "left", 
          yanchor = "bottom",
          sizing = "stretch"
        ),
        ...,
        xaxis = list(
          range = rastObj$x_range,
          title = var_names["x"]
        ),
        yaxis = list(
          range = rastObj$y_range,
          title = var_names["y"]
        ),
        plot_bgcolor = rastObj$background
      )
  }
  return(p)
}