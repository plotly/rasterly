#' @title rasterizer
#' @description Display large data set in R
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
#'        rasterizer(mapping = aes(x = x, y = y, colour = category)) %>%
#'        aggregation_points(layout = "weighted") -> ds1
#'
#'      data.frame(x = x, y = y, category = category) %>%
#'        rasterizer(mapping = aes(x = x, y = y, colour = category)) %>%
#'        aggregation_points(layout = "cover") -> ds2
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
rasterizer <- function(data = NULL,
                      mapping = ggplot2::aes(),
                      ...,
                      plot_width = 600, plot_height = 600,
                      x_range = NULL, y_range = NULL,
                      background = "#FFFFFF",
                      colour_map = c('lightblue','darkblue'),
                      colour_key = NULL,
                      reduction_func = NULL,
                      show_raster = TRUE,
                      layout = c("weighted", "cover")) {
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

  # convert to string
  reduction_func <- if(is.null(reduction_func)) "" else {
    if(is.character(reduction_func)) reduction_func
    else if(is.function(reduction_func)) deparse(substitute(reduction_func))
    else stop("unknown `reduction_func` type")
  }

  args <- list(
    data = data,
    mapping = mapping,
    plot_width = plot_width,
    plot_height = plot_height,
    x_range = x_range,
    y_range = y_range,
    background = background,
    colour_map =  get_colour_map(p = NULL, colour_map = colour_map, ...),
    colour_key = colour_key,
    show_raster = show_raster,
    reduction_func = reduction_func,
    layout = match.arg(layout),
    ...
  )
  attr(args, 'class') <- c('rasterizer')
  return(args)
  invisible()
}

is.rasterizer <- function(x) {
  inherits(x, "rasterizer")
}

#' @export
aggregation_points <- function(p,
                               data = NULL,
                               mapping = aes(),
                               ...,
                               reduction_func = NULL,
                               how = 'eq_hist',  extend_rate = NULL,
                               glyph = NULL) {

  # argument check
  if(missing(p)) stop("'rasterizer' is missing")
  if(!is.rasterizer(p)) stop(paste(deparse(substitute(p)), "is not a 'rasterizer' object"))

  if (!missing(mapping) && !inherits(mapping, "uneval")) {
    stop("Mapping should be created with `aes()`.", call. = FALSE)
  }

  plot_width <- p$plot_width
  plot_height <- p$plot_height
  x_range <- p$x_range
  y_range <- p$y_range
  show_raster <- p$show_raster
  background <-  p$background

  colour_map <- get_colour_map(p = p, ...)
  alpha <- get_alpha(p = p, ...)
  span <- get_span(p = p, ...)
  layout <- get_layout(p = p, ...)

  # convert to string
  reduction_func <- if(is.null(reduction_func)) "" else {
    if(is.character(reduction_func)) reduction_func
    else if(is.function(reduction_func)) deparse(substitute(reduction_func))
    else stop("unknown `reduction_func` type")
  }
  if(reduction_func == "") {
    reduction_func <- p$reduction_func
  }
  # for S3 method
  class(reduction_func) <- reduction_func

  data <- if(!is.null(data)) data else p$data
  if(is.null(data)) stop("no 'data' found")
  if(!is.data.frame(data)) stop(paste(deparse(substitute(data)), "is not a data frame"), call. = FALSE)

  if(rlang::is_empty(mapping)) mapping <- p$mapping
  if(rlang::is_empty(mapping)) stop("Miss aesthetics")

  aesthetics <- get_aesthetics(data, mapping)

  # add extend_rate and size in aesthetics
  aesthetics$extend_rate <- get_extend_rate(p = p, extend_rate = extend_rate)
  aesthetics$pixel_share <- get_size(p = p, ...)
  aesthetics$glyph <- get_glyph(p = p, glyph = glyph)

  start_time <- Sys.time()
  if(is.null(x_range)) {
    x_range <- c(min(aesthetics$x$value), max(aesthetics$x$value))
  }

  if(is.null(y_range)) {
    y_range <- c(min(aesthetics$y$value), max(aesthetics$y$value))
  }
  end_time <- Sys.time()
  print(paste("get range time:", end_time - start_time))

  # "aggregation" is a list of matrices modified by some specific reduction function
  # if the length of "aggregation" is one, which means it is a single image
  # else it should be multiple images
  start_time <- Sys.time()
  agg <- get_aggregation(
    plot_width = plot_width, plot_height = plot_height,
    aesthetics = aesthetics,
    x_range = x_range, y_range = y_range,
    func = reduction_func,
    ...
  )
  end_time <- Sys.time()
  print(paste("get_aggregation time:", end_time - start_time))

  len_agg <- length(agg)
  if(len_agg == 0) stop("No graphics are found")
  else if(len_agg == 1) is_categorical <- FALSE
  else is_categorical <- TRUE

  # show raster or not
  if(show_raster) {
    start_time <- Sys.time()
    image <- p$image
    if(is_categorical) {
      image <- get_raster_3D(L = agg, colour_key = get_colour_key(p = p, n = len_agg, ...),
                             layout = layout, span = span,
                             zeroIgnored = TRUE, image = image, background = background,
                             alpha = alpha)
    } else {
      image <- get_raster_2D(M = agg[[1]], colour_map = colour_map, span = span,
                             zeroIgnored = TRUE, image = image, background = background,
                             alpha = alpha, layout = layout)
    }
    p$image <- image
    end_time <- Sys.time()
    print(paste("get raster time:", end_time - start_time))
  }

  p$agg <- c(p$agg, agg)
  p$limits <- c(
    p$limits,
    list(
      x_range = x_range,
      y_range = y_range
    )
  )

  structure(p, class = unique(c("aggregation", class(p))))
  return(p)
  invisible()
}

is.aggregation <- function(x) {
  inherits(x, "aggregation")
}

#' @export
aes <- ggplot2::aes

#' @export
`%>%` <- magrittr::`%>%`
