#' @title ggRasterly
#' @description Display large data set in \code{ggplot}.
#' @inheritParams rasterly
#' @param alpha The transparency of points, from 0 to 1.
#' @param shape The shape of points, see \link{pch}.
#' @param stroke The stroke of points (size).
#' @return a `ggplot` object
#' 
#' @seealso \link{plotRasterly}
#' 
#' @examples
#' \dontrun{
#'if(requireNamespace("ggplot2") && requireNamespace("data.table") && 
#'   requireNamespace("lubridate")) {
#'  # Load data
#'  url1 <- "https://raw.githubusercontent.com/plotly/datasets/master/uber-rides-data1.csv"
#'  ridesRaw_1 <-  url1 %>%
#'    data.table::fread(stringsAsFactors = FALSE)
#'  url2 <- "https://raw.githubusercontent.com/plotly/datasets/master/uber-rides-data2.csv"
#'  ridesRaw_2 <-  url2 %>%
#'    data.table::fread(stringsAsFactors = FALSE)
#'  url3 <- "https://raw.githubusercontent.com/plotly/datasets/master/uber-rides-data3.csv"
#'  ridesRaw_3 <-  url3 %>%
#'    data.table::fread(stringsAsFactors = FALSE) 
#'    
#'  ridesDf <- list(ridesRaw_1, ridesRaw_2, ridesRaw_3) %>%
#'    data.table::rbindlist()
#'    
#'  time <- lubridate::ymd_hms(ridesDf$`Date/Time`)
#'  ridesDf <-  ridesDf[, 'Date/Time':=NULL][, list(Lat,
#'                                                  Lon,
#'                                                  hour = lubridate::hour(time),
#'                                                  month = lubridate::month(time),
#'                                                  day = lubridate::day(time))]
#'
#'  #### quick start
#'  ggRasterly(data = ridesDf, 
#'             mapping = aes(x = Lat, y = Lon, color = hour),
#'             color = hourColors_map
#'  ) + 
#'  labs(title = "New York Uber",
#'       subtitle = "Apr to Sept, 2014",
#'       caption = "Data from https://raw.githubusercontent.com/plotly/datasets/master")
#'  }
#' }
#' @export
ggRasterly <- function(data = NULL,
                       mapping = aes(),
                       ...,
                       plot_width = 600, plot_height = 600,
                       x_range = NULL, y_range = NULL,
                       background = "white",
                       color = NULL,
                       show_raster = TRUE,
                       drop_data = FALSE,
                       variable_check = FALSE,
                       alpha = 0.5,
                       shape = 19,
                       stroke = 0.1) {
  
  if(!show_raster) return(ggplot2::ggplot())
  
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
  
  len <- max(plot_width, plot_height)
  if("color" %in% names(mapping)) {
    uni_color <- rlang::eval_tidy(mapping$color, data) %>% 
      unique() %>% 
      as.factor() # this line may be expensive
    if(len > length(uni_color)) {
      color <- rep_len(uni_color, length.out = len)
    } else {
      len <- length(uni_color)
    }

    ggObj <- ggplot2::ggplot() + 
      # all the rest is for legend
      # This is a hack!
      # Since geom_blank() does not display the right color in legend
      ggplot2::geom_point(data = data.frame(x = seq(rastObj$x_range[1], rastObj$x_range[2], length.out = len),
                                            y = seq(rastObj$y_range[1], rastObj$y_range[2], length.out = len),
                                            color = color),
                          mapping = aes(x = x, y = y, color = color),
                          alpha = 0) + 
      ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(alpha=1))) + 
      ggplot2::scale_colour_manual(
        values = stats::setNames(rastObj$colors[[1]][seq_len(length(uni_color))], uni_color)
      )
  } else {
    ggObj <- ggplot2::ggplot()
  }
  
  imageData <- image2data(x = rastObj$image, 
                          x_range = rastObj$x_range,
                          y_range = rastObj$y_range,
                          background = rastObj$background)
  pointLayer <- if(shape %in% 21:24) {
    ggplot2::geom_point(data = imageData,
                        mapping = aes(x = x, y = y),
                        fill = imageData$color,
                        alpha = alpha,
                        stroke = stroke,
                        shape = shape)
  } else {
    ggplot2::geom_point(data = imageData,
                        mapping = aes(x = x, y = y),
                        color = imageData$color,
                        alpha = alpha,
                        stroke = stroke,
                        shape = shape)
  }
  ggObj <- ggObj + 
    pointLayer + 
    ggplot2::xlab(sub("~", "", rlang::expr_text(mapping$x))) + 
    ggplot2::ylab(sub("~", "", rlang::expr_text(mapping$y)))
  
  return(ggObj)
}

gg_pretty <- function(ggObj) {
  
  build <- ggplot2::ggplot_build(ggObj)
  panel_params <- build$layout$panel_params
  
  lapply(panel_params, 
         function(panel_param) {
           list(
             x_major = panel_param$x.major_source,
             x_minor = panel_param$x.minor_source,
             x_range = panel_param$x.range,
             y_major = panel_param$y.major_source,
             y_minor = panel_param$y.minor_source,
             y_range = panel_param$y.range
           )
         })
}