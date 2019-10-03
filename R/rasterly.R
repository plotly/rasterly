#' @title Rasterly
#' @description Set initials for rasterly
#' @param data Dataset to use for plot. If not provided, data must be supplied in each layer of the plot.
#' Since "rasterly" is used for large dataset, "\link[data.table]{data.table}" is recommended.
#' @param mapping Default list of aesthetic mappings to use for plot. The same with `ggplot2` \link[aes]{aes}.
#' See details.
#' @param ... Other arguments can be passed to layers.
#' @param plot_width The width of image, must be positive integer. Higher value indicates higher resolution.
#' @param plot_height The height of image, must be positive integer. Higher value indicates higher resolution.
#' @param x_range The range of x; It can be used to clip the image. Also, to large dataset, provided `x_range`
#' can help to speed code
#' @param y_range The range of y; It can be used to clip the image. Also, to large dataset, provided `y_range`
#' can help to speed code
#' @param background Background colour of this image.
#' @param colour_map Colour used to map in each pixel. The `colour_map` would be extended by linear interpolation
#' independently for RGB. The darkness of the colour depends on the aggregation matrix value.
#' @param colour_key Used for categorical variable. In general, `colour_key` would be called when "colour"
#' is set in `aes()`
#' @param show_raster Logical value. Whether show raster or not.
#' @param drop_data Logical value. In general, data passed in has large size which may take too many rooms. When data is 
#' manipulated by given `aes()`, original data can be dropped via function `remove()` by setting `drop_data = TRUE`. 
#' See details for more information.
#' @param variable_check Logical value to drop unused columns. Setting `TRUE` can help to 
#' save more space by sacrificing speed.
#' 
#' @return An environment wrapped by a list
#' 
#' @note Call `rasterly()` alone does not generate anyting. `rasterize_...()` layers need to be attached. 
#' More info can be found in \href{https://github.com/plotly/rasterly/blob/master/README.md}{README.md}
#'
#' @seealso \link{rasterize_points}, \link{rasterly_build}, \link{[.rasterly}, \link{[<-.rasterly}
#' @details 
#' \itemize{
#'  \item{}{In package "rasterly", only five aesthetics can be passed in `aes()` so far, "x", "y", "on", "colour" and "size".
#' variable "on" represents the reduction function works "on" which column.}
#'  \item{}{`drop_data` can help save space, especially to extremly large dataset, 
#' however, drop original data can cause layers fail to set new `aes()`.}
#' }
#'
#' @useDynLib rasterly
#' @import Rcpp
#' @import rlang
#' @importFrom grDevices rgb col2rgb hcl extendrange
#' @importFrom magrittr '%>%'
#' @importFrom stats ecdf approx setNames
#' @importFrom ggplot2 aes
#' @importFrom data.table data.table
#' @importFrom compiler cmpfun

#' @export
rasterly <- function(data = NULL,
                       mapping = aes(),
                       ...,
                       plot_width = 600, plot_height = 600,
                       x_range = NULL, y_range = NULL,
                       background = "white",
                       colour_map = c('lightblue','darkblue'),
                       colour_key = NULL,
                       show_raster = TRUE,
                       drop_data = FALSE,
                       variable_check = FALSE) {
  # argument check
  if(!missing(mapping) && !inherits(mapping, "uneval")) {
    stop("Mapping should be created with `aes()`.", call. = FALSE)
  }
  
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
    
    start_time <- Sys.time()
    aesthetics <- get_aesthetics(data, mapping, variable_check, ...)
    end_time <- Sys.time()
    print(paste("get aesthetics time:", end_time - start_time))
    
    start_time <- Sys.time()
    range <- get_range(x_range = x_range, 
                       y_range = y_range,
                       x = aesthetics$x, 
                       y = aesthetics$y)
    x_range <- range$x_range
    y_range <- range$y_range
    end_time <- Sys.time()
    print(paste("get range time:", end_time - start_time))
    
    if(drop_data) {
      remove(data)
      message("The layer with new mapping aesthetics may do not work")
    }
  }
  
  # make sure all arguments exist in `rasterly` environment
  args <- list(...)
  reduction_func <- args$reduction_func
  layout <- args$layout
  glyph <- args$glyph
  group_by_data_table <- args$group_by_data_table
  
  p <- structure(
    list(
      rasterly_env = environment()
    ),
    class = c("rasterly")
  )
  return(p)
  invisible()
}