#' @title plotRasterly
#' @description Display large data set in `plotly`
#' @inheritParams ggRasterly
#' @param as_image Logical value. If \code{FALSE}, image raster will be transformed into a data frame, hence a points layer
#' would be pipped on \code{plotly}; conversely, a raster layer will be added.
#' @param sizing It affects only with \code{as_image = TRUE}. Specifies which dimension of the image to constrain.
#'  One of "stretch" "fill", "contain". see https://plot.ly/r/reference/#Layout_and_layout_style_objects
#' @return a `plotly` widget
#' 
#' @seealso \link{ggRasterly}
#' 
#' @export
#' 
#' @examples 
#' \dontrun{
#'  library(rasterly)
#'  if(requireNamespace("plotly") && 
#'     requireNamespace("data.table") && 
#'     requireNamespace("lubridate")) {
#'    # Load data
#'  url1 <- "https://raw.githubusercontent.com/plotly/datasets/master/uber-rides-data1.csv"
#'  ridesRaw_1 <-  url1 %>%
#'    data.table::fread(stringsAsFactors = FALSE)
#'  url2 <- "https://raw.githubusercontent.com/plotly/datasets/master/uber-rides-data2.csv"
#'  ridesRaw_2 <-  url2 %>%
#'    data.table::fread(stringsAsFactors = FALSE)
#'  url3 <- "https://raw.githubusercontent.com/plotly/datasets/master/uber-rides-data3.csv"
#'  ridesRaw_3 <-  url3 %>%
#'    data.table::fread(stringsAsFactors = FALSE) 
#'  ridesDf <- list(ridesRaw_1, ridesRaw_2, ridesRaw_3) %>% 
#'    data.table::rbindlist()
#'        
#'  time <- lubridate::ymd_hms(ridesDf$`Date/Time`)
#'  ridesDf <-  
#'    ridesDf[, 'Date/Time':=NULL][, list(Lat, 
#'               Lon,                                               
#'               hour = lubridate::hour(time),                                                
#'               month = lubridate::month(time),
#'               day = lubridate::day(time))]
#'  # A point layer is added
#'  plotRasterly(data = ridesDf, 
#'               mapping = aes(x = Lat, y = Lon, color = hour),
#'               color = hourColors_map,
#'               as_image = FALSE)
#'   # An image layer is added       
#'   plotRasterly(data = ridesDf, 
#'                mapping = aes(x = Lat, y = Lon, color = hour),
#'                color = hourColors_map,
#'                as_image = TRUE)
#'   
#'  }
#' }
plotRasterly <- function(data = NULL,
                         mapping = aes(),
                         ...,
                         plot_width = 400, plot_height = 400,
                         x_range = NULL, y_range = NULL,
                         background = "white",
                         color = NULL,
                         show_raster = TRUE,
                         drop_data = FALSE,
                         variable_check = FALSE,
                         alpha = 0.5,
                         shape = 19,
                         stroke = 0.5, 
                         as_image = FALSE, 
                         sizing = c("stretch", "fill", "contain")) {
  
  if(!show_raster) return(plotly::plot_ly())
  
  if(as_image) {
    
    mapping <- rename_mapping(mapping)
    rastObj <- rasterly(data = data,
                        mapping = mapping,
                        ...,
                        plot_width = plot_width, 
                        plot_height = plot_height,
                        x_range = x_range, 
                        y_range = y_range,
                        background = background,
                        color = color,
                        show_raster = show_raster,
                        drop_data = drop_data,
                        variable_check = variable_check) %>% 
      rasterly_points() %>% 
      rasterly_build()
    
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
  } else {
    p <- ggRasterly(
      data = data,
      mapping = mapping,
      ...,
      plot_width = plot_width, 
      plot_height = plot_height,
      x_range = x_range, y_range = y_range,
      background = background,
      color = color,
      show_raster = show_raster,
      drop_data = drop_data,
      variable_check = variable_check,
      alpha = alpha,
      shape = shape,
      stroke = stroke
    ) %>% 
      plotly::ggplotly()
  }
  return(p)
}
