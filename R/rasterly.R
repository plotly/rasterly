#' Easily and rapidly generate raster image data with support for Plotly.js 
#' @description Create a rasterly object, to which aggregation layers may be added. This function is the first step in the process
#' to generate raster image data using the \code{rasterly} package.
#' @param data Dataset to use for generating the plot. If not provided, data must be supplied in each layer of the plot.
#' For best performance, particularly when processing large datasets, use of \link[data.table]{data.table} is recommended.
#' @param mapping Default list of aesthetic mappings to use for plot. The same with \code{ggplot2} \link[ggplot2]{aes}.
#' See details.
#' @param ... Other arguments which will be passed through to layers.
#' @param plot_width Integer. The width of the image to plot; must be a positive integer. A higher value indicates a higher resolution.
#' @param plot_height Integer. The height of the image to plot; must be a positive integer. A higher value indicates a higher resolution.
#' @param x_range Vector of type numeric. The range of \code{x}; it can be used to clip the image. For larger datasets, providing \code{x_range}
#' may result in improved performance.
#' @param y_range Vector of type numeric. The range of \code{y}; it can be used to clip the image. For larger datasets, providing \code{y_range}
#' may result in improved performance.
#' @param background Character. The background color of the image to plot.
#' @param color Vector of type character. It will determine this color vector is a \code{color_map} or \code{color_key} automatically.
#' \itemize{
#'  \item{}{color_map: It has Color(s) used to draw each pixel. The \code{color_map} is extended by linear interpolation
#' independently for RGB. The darkness of the mapped color depends upon the values of the aggregation matrix.
#'  }
#'  \item{}{color_key: Vector of type character. The \code{color_key} is used for categorical variables; 
#'  it is passed when the \code{color} aesthetic is provided.
#' }
#' }
#' @param show_raster Logical. Should the raster be displayed?
#' @param drop_data Logical. When working with large datasets, drops the original data once processed according to the provided
#' \code{aes()} parameters, using the \code{remove()} function. See details for additional information.
#' @param variable_check Logical. If \code{TRUE}, drops unused columns to save memory; may result in reduced performance.
#' 
#' @return An environment wrapped by a list which defines the properties of the raster data to be generated.
#' 
#' @note Calling \code{rasterly()} without providing \code{rasterly_...()} layers has no effect.
#' More info can be found in \href{https://github.com/plotly/rasterly/blob/master/README.md}{README.md}
#'
#' @seealso \link{rasterly_points}, \link{rasterly_build}, \link{[.rasterly}, \link{[<-.rasterly}
#' \link{ggRasterly}, \link{plotRasterly}
#' @details 
#' \itemize{
#'  \item{}{The rasterly package currently supports five aesthetics via \code{aes()}: \code{x}, \code{y}, \code{on}, \code{color}, and \code{size}.
#'  The "on" aesthetic specifies the variable upon which the reduction function should be applied to generate the raster data.
#'  }
#'  \item{}{\code{drop_data} can help save space, particularly when large datasets are used. However, dropping the original dataset
#'  may result in errors when attempting to set or update \code{aes()} parameters within rasterly layers.
#' }
#' }
#' 
#' @examples 
#' \dontrun{
#'  if(requireNamespace("data.table")) {
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
#'  ridesDf %>%
#'        rasterly(mapping = aes(x = Lat, y = Lon)) %>%
#'        rasterly_points()
#'  }}
#' 
#' @useDynLib rasterly
#' @import Rcpp
#' @import rlang
#' @import methods
#' @importFrom grDevices rgb col2rgb hcl extendrange as.raster
#' @importFrom magrittr '%>%'
#' @importFrom stats ecdf approx setNames na.omit
#' @importFrom ggplot2 aes
#' @importFrom data.table data.table
#' @export
rasterly <- function(data = NULL,
                     mapping = aes(),
                     ...,
                     plot_width = 600, plot_height = 600,
                     x_range = NULL, y_range = NULL,
                     background = "white",
                     color = NULL,
                     show_raster = TRUE,
                     drop_data = FALSE,
                     variable_check = FALSE) {
  mapping <- rename_mapping(mapping)
  
  stopifnot(
    exprs = {
      is.numeric(plot_width) && plot_width > 0
      is.numeric(plot_height) && plot_height > 0
    }
  )
  plot_width <- round(plot_width)
  plot_height <- round(plot_height)
  
  if(!is.null(x_range)) {
    stopifnot(
      exprs = {
        length(x_range) == 2 && is.numeric(x_range)
      }
    )
  }
  
  if(!is.null(y_range)) {
    stopifnot(
      exprs = {
        length(y_range) == 2 && is.numeric(y_range)
      }
    )
  }
  
  if(!is.null(data) && !rlang::is_empty(mapping)) {
    aesthetics <- get_aesthetics(data, mapping, variable_check, ...)
    range <- get_range(x_range = x_range, 
                       y_range = y_range,
                       x = aesthetics$x, 
                       y = aesthetics$y)
    x_range <- range$x_range
    y_range <- range$y_range
    
    if(drop_data) {
      remove(data)
      message("drop_data is TRUE; removing the source dataset. Attempts to pass new aes() parameters to layers may generate an error.")
    }
  }
  
  # make sure all arguments exist in `rasterly` environment
  args <- list(...)
  if(!is.null(args$color_map)) {
    warning("`color_map` is deprecated now. Please use `color` instead.", call. = FALSE)
    color_map <- args$color_map
    color <- color_map
  }
  if(!is.null(args$color_key)) {
    warning("`color_key` is deprecated now. Please use `color` instead.", call. = FALSE)
    color_key <- args$color_key
    color <- color_key
  }
  
  rastObj <- structure(
    list(
      rasterly_env = environment()
    ),
    class = c("rasterly")
  )
  invisible(return(rastObj))
}
