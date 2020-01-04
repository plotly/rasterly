#' @title Rasterly plot
#' @name rplot
#' @description \code{rplot} is created to generate \code{rasterly} plot quickly but with base 
#' \link{plot} design. It is convenient but lack of flexibility and \link{rasterly} is highly
#' recommended.
#' @param x,y Coordinates x, y for the plot. 
#' @param ... Other aesthetics to pass through, like \code{color}, \code{size} and \code{on}.
#' @export
#' @seealso \link{rasterly} \link{rasterly_points}
rplot <- function(x, y = NULL, ...) {
  UseMethod("rplot")
}

#' @inherit rplot
#' @rdname rplot
#' @param plot_width Integer. The width of the image to plot; must be a positive integer. A higher value indicates a higher resolution.
#' @param plot_height Integer. The height of the image to plot; must be a positive integer. A higher value indicates a higher resolution.
#' @param x_range Vector of type numeric. The range of \code{x}; it can be used to clip the image. For larger datasets, providing \code{x_range}
#' may result in improved performance.
#' @param y_range Vector of type numeric. The range of \code{y}; it can be used to clip the image. For larger datasets, providing \code{y_range}
#' may result in improved performance.
#' @param background Character. The background color of the image to plot.
#' @param reduction_func Function. A reduction function is used to aggregate data points into their pixel representations. Currently
#' supported reduction operators are \code{sum}, \code{any}, \code{mean}, \code{m2}, \code{first}, \code{last}, \code{min} and \code{max}. Default is \code{sum}. See details.
#' @param layout Character. The method used to generate layouts for multiple images. The default is \code{weighted}. Useful for categorical
#' data (i.e. "color" is provided via \code{aes()}). \code{weighted} specifies that the final raster should be a weighted combination of each
#' (categorical) aggregation matrix. Conversely, \code{cover} indicates that the afterwards objects will be drawn on top of 
#' the previous ones.
#' @param glyph Character. Currently, only "circle" and "square" are supported; as the \code{size} of the pixels increases, how should they
#' spread out -- should the pattern be circular or square? Other glyphs may be added in the future.
#' 
#' @export
#' @examples 
#' if(requireNamespace("ggplot2")) {
#'   library(ggplot2)
#'   # `color` represents a variable here
#'   with(diamonds, 
#'        rplot(x = carat, y = price, color = color)
#'   )
#'   # `color` represents an actual color vector
#'   with(diamonds, 
#'        rplot(x = carat, y = price, color = fire_map)
#'   )
#' }
rplot.default <- function(x, y = NULL, ..., 
                          plot_width = 600, plot_height = 600,
                          x_range = NULL, y_range = NULL,
                          background = "white",
                          reduction_func = NULL,
                          layout = NULL,
                          glyph = NULL) {
  
  xy <- grDevices::xy.coords(x, y)
  data <- data.table::data.table(x = xy$x, y = xy$y)
  mapping <- aes(x = x, y = y)
  n <- dim(data)[1]
  
  args <- list(...)
  
  for(aes_name in setdiff(rasterly_aesthetics, c("x", "y"))) {
    var <- args[[aes_name]]

    if(length(var) == n) {
      data <- data[, rlang::expr_name(aes_name) := var]
      aes_names <- names(mapping)
      mapping <- mbind(rlang::quo(!!rlang::sym(aes_name)), mapping) %>% 
        stats::setNames(., c(aes_name, aes_names))
      args[[aes_name]] <- NULL
    }
  }

  r <- do.call(
    rasterly,
    c(
      list(
        data = data,
        mapping = mapping,
        plot_width = plot_width,
        plot_height = plot_height,
        background = background,
        x_range = x_range,
        y_range = y_range
      ),
      args
    )
  )  %>% 
    rasterly_points(reduction_func = reduction_func,
                    layout = layout,
                    glyph = glyph) %>% 
    rasterly_build()
  
  print(r)
  invisible()
}
