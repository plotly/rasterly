#' @title Canvas
#' @description Set canvas for rasterizer
#' @param data Dataset to use for plot. If not provided, data must be supplied in each layer of the plot.
#' @param mapping Default list of aesthetic mappings to use for plot. The same with `ggplot2`\link[aes]{aes}.
#' See details.
#' @param ... Other arguments can be passed to layer.
#' @param plot_width The width of image, must be positive integer. Higher value indicates higher resolution.
#' @param plot_height The height of image, must be positive integer. Higher value indicates higher resolution.
#' @param x_range The range of x; It can be used to clip the image. Also, to large dataset, provided `x_range`
#' can help to speed code
#' @param y_range The range of y; It can be used to clip the image. Also, to large dataset, provided `y_range`
#' can help to speed code
#' @param background Background colour of this image. All layers share one background colour.
#' @param colour_map Colour used to map in each pixel. The `colour_map` would be extended by linear interpolation
#' independently for RGB. The darkness of the colour depends on the aggregation matrix value.
#' @param colour_key Used for categorical variable. In general, `colour_key` would be called when "colour"
#' is set in `aes()`
#' @param reduction_func A reduction operator function is used when aggregating datapoints
#' into a given pixel. The supported reduction operators are `sum`, `any`, `mean`, `first` and `last`.
#' More is coming
#' @param show_raster Logical value. Whether show raster or not
#' @param layout The way to layout multiple images. It is useful when data is catergorical("colour" is set in `aes()`).
#' The default setting is "weighted", which means the final raster is the weighted combination of each category
#' aggregation matrix. Also, we can "cover" each raster by the order of unique categories.
#'
#'
#' @details In `rasterizer`, only five arguments can be passed in `aes()`, "x", "y", "on", "colour" and "size".
#' variable "on" represents the reduction function works "on" which column.
#'
#' @useDynLib rasterizer
#' @import Rcpp
#' @import rlang
#' @import grDevices
#' @importFrom magrittr '%>%'
#' @importFrom stats ecdf approx setNames
#' @importFrom ggplot2 aes
#' @importFrom data.table data.table
#' @importFrom compiler cmpfun
#'
#'
#' @examples
#' \dontrun{
#'    library(rasterizer)
#'    if(requireNamespace("grid") && requireNamespace("gridExtra")) {
#'      x <- rnorm(1e7)
#'      y <- rnorm(1e7)
#'      category <- sample(1:5, 1e7, replace = TRUE)
#'      data.frame(x = x, y = y, category = category) %>%
#'        canvas(mapping = aes(x = x, y = y, colour = category)) %>%
#'        aggregation_points(layout = "weighted") %>%
#'        rasterizer() -> ds1
#'
#'      data.frame(x = x, y = y, category = category) %>%
#'        canvas(mapping = aes(x = x, y = y, colour = category)) %>%
#'        aggregation_points(layout = "cover") %>%
#'        rasterizer() -> ds2
#'
#'      grid::grid.newpage()
#'      gridExtra::grid.arrange(
#'         grobs = list(grid::rasterGrob(ds1$image), grid::rasterGrob(ds2$image)),
#'         ncol = 2,
#'         top = "'weighted' layout versus 'cover' layout"
#'      )
#'    }
#' }

#' @export
canvas <- function(data = NULL,
                   mapping = aes(),
                   ...,
                   plot_width = 600, plot_height = 600,
                   x_range = NULL, y_range = NULL,
                   background = "#FFFFFF",
                   colour_map = c('lightblue','darkblue'),
                   colour_key = NULL,
                   show_raster = TRUE,
                   remove_data = FALSE,
                   variable_check = FALSE) {
  # argument check
  if (!missing(mapping) && !inherits(mapping, "uneval")) {
    stop("Mapping should be created with `aes()`.", call. = FALSE)
  }
  
  stopifnot(
    exprs = {
      is.numeric(plot_width) && plot_width > 0
      is.numeric(plot_height) && plot_height > 0
    }
  )
  
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
  
  if(plot_width < 10 || plot_height < 10) stop('plot region is too small',
                                               'please pick a larger plot_width or plot_height',
                                               call. = FALSE)
  
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
    
    if(remove_data) {
      remove(data)
      message("The layer with new mapping aesthetics may do not work")
    }
  }
  
  p <- structure(
    list(
      canvas_env = environment()
    ),
    class = c("canvas", "rasterizer")
  )
  return(p)
  invisible()
}