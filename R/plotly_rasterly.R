#' @title `rasterly` to `plotly`
#' @description Display raster image via `plotly`
#' @param rastObj A rasterly object
#' @param as_heatmap Draw `plotly` by adding heatmap layer. See \code{add_rasterly}
#' @param scaling It could be an artificial function or a scaling way ("log", "origin") 
#' @param sizing Specifies which dimension of the image to constrain. One of "stretch" "fill", "contain"
#' @param ... Arguments to the layout object. For documentation, 
#' see https://plot.ly/r/reference/#Layout_and_layout_style_objects
#' 
#' @export
#' 
#' @examples 
#' \dontrun{
#'    library(rasterly)
#'    if(requireNamespace("plotly") && 
#'       requireNamespace("data.table") && 
#'       requireNamespace("lubridate")) {
#'      # Load data
#'      ridesRaw_1 <- "https://raw.githubusercontent.com/plotly/datasets/
#'      master/uber-rides-data1.csv" %>%
#'        data.table::fread(stringsAsFactors = FALSE)
#'      ridesRaw_2 <- "https://raw.githubusercontent.com/plotly/datasets/
#'      master/uber-rides-data2.csv" %>% 
#'        data.table::fread(stringsAsFactors = FALSE)
#'      ridesRaw_3 <- "https://raw.githubusercontent.com/plotly/datasets/
#'      master/uber-rides-data3.csv"  %>% 
#'        data.table::fread(stringsAsFactors = FALSE)
#'      ridesDf <- list(ridesRaw_1, ridesRaw_2, ridesRaw_3) %>% 
#'        data.table::rbindlist()
#'        
#'      time <- ymd_hms(ridesDf$`Date/Time`)
#'      ridesDf <-  
#'      ridesDf[, 'Date/Time':=NULL][, list(Lat, 
#'                 Lon,                                               
#'                 hour = hour(time),                                                
#'                 month = month(time),
#'                 day = day(time))]
#'      
#'      max_x <- max(ridesDf$Lat)
#'      min_x <- min(ridesDf$Lat)
#'      max_y <- max(ridesDf$Lon)
#'      min_y <- min(ridesDf$Lon)
#'      ridesDf %>% 
#'        rasterly(background = "black") %>%
#'        rasterize_points(xlim = c(min_x, (min_x+max_x)/2), 
#'                         ylim = c(min_y, max_y),
#'                         mapping = aes(x = Lat, y = Lon),
#'                         color_map = fire) %>% 
#'        rasterize_points(xlim = c((min_x+max_x)/2, max_x), 
#'          ylim = c(min_y, max_y),
#'          mapping = aes(x = Lat, y = Lon, color = hour),
#'          color_key = hourColours) %>% 
#'     rasterize_build() %>%
#'     plotly.rasterly(title = "New York Uber Rides")
#'    }
#' }
plotly.rasterly <- function(rastObj, 
                              as_heatmap = FALSE, 
                              scaling = NULL, 
                              sizing = c("stretch", "fill", "contain"),
                              ...) {
  
  if(missing(rastObj) || !is.rasterly(rastObj)) stop("No 'rasterly' object.", call. = FALSE)
  if(is.rasterizeLayer(rastObj) && !is.rasterly_build(rastObj)) {
    # to a 'rasterly_build' object
    rastObj['show_raster', which = 1] <- !as_heatmap
    rastObj <- rasterly_build(rastObj)
  }
  
  if(as_heatmap) {
    if(sum(lengths(rastObj$agg)) > 1) 
      message("More than one aggregation matrix is detected. 
            Set `as_heatmap = FALSE` is recommended.")
    z <- rastObj$agg[[1]][[1]]
    dimZ <- dim(z)
    y <- seq(rastObj$y_range[1], rastObj$y_range[2], length.out = dimZ[1])
    x <- seq(rastObj$x_range[1], rastObj$x_range[2], length.out = dimZ[2])
    
    scaling <- scaling %||% {
      message("The default scaling is 'log'.")
      "log"
    }
    if(is.function(scaling)) {
      z <- do.call(scaling, 
                   list(z = z))
    } else {
      if(!is.character(scaling)) stop("'scaling' can be either 'function' or 'character'")
      switch(scaling, 
             "log" = {
               z <- matrix(log(z + 1), nrow = dimZ[1])
             }, 
             "origin" = NULL)
    }
    
    p <- plotly::plot_ly(x = x, y = y) %>% 
      plotly::add_heatmap(z = z)
  } else {
    image <- rastObj$image
    if(is.null(image)) 
      stop("No image is found. Consider set `show_raster = TRUE` in `rasterly()`?", call. = FALSE)
    
    var_names <- unlist(rastObj$variable_names)

    sizing <- match.arg(sizing)
    
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
          sizing = sizing
        ),
        ...,
        xaxis = list(
          range = rastObj$x_range,
          title = get_varnames(var_names, "x")
        ),
        yaxis = list(
          range = rastObj$y_range,
          title = get_varnames(var_names, "y")
        ),
        plot_bgcolor = rastObj$background
      )
  }
  return(p)
}
