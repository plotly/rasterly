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

is.canvas <- function(x) {
  inherits(x, "canvas")
}

is.rasterizer <- function(x) {
  inherits(x, "rasterizer")
}

#' @export
aggregation_points <- function(rastObj,
                               data = NULL,
                               mapping = aes(),
                               ...,
                               xlim = NULL,
                               ylim = NULL,
                               max_size = NULL,
                               reduction_func = NULL,
                               layout = NULL,
                               glyph = NULL,
                               group_by_data_table = NULL) {
  
  # argument check
  if(missing(rastObj) || !is.rasterizer(rastObj)) stop("No 'rasterizer' object", call. = FALSE)
  
  if (!missing(mapping) && !inherits(mapping, "uneval")) {
    stop("Mapping should be created with `aes()`.", call. = FALSE)
  }
  
  background <-  get_background(envir = rastObj$canvas_env, ...)
  colour_map <- get_colour_map(envir = rastObj$canvas_env, ...)
  alpha <- get_alpha(envir = rastObj$canvas_env, ...)
  span <- get_span(envir = rastObj$canvas_env, ...)
  layout <- get_layout(envir = rastObj$canvas_env, layout = layout)
  glyph <- get_glyph(envir = rastObj$canvas_env, glyph = glyph)
  group_by_data_table <- get_group_by_data_table(envir = rastObj$canvas_env, 
                                                 group_by_data_table = group_by_data_table)
  
  reduction_func <- if(is.null(reduction_func)) {
    func <- .get("reduction_func", envir = rastObj$canvas_env)
    ifelse(is.null(func), "", func)
  } else {
    if(is.character(reduction_func)) reduction_func
    else if(is.function(reduction_func)) deparse(substitute(reduction_func))
    else stop("unknown `reduction_func` type", call. = FALSE)
  }
  if(reduction_func == "") {
    reduction_func <- "sum"
  }
  # for S3 method
  class(reduction_func) <- reduction_func

  if(!is.null(data)) {
    # new input data in this layer
    if(rlang::is_empty(mapping)) {
      mapping <- .get("mapping", envir = rastObj$canvas_env)
    }
    aesthetics <- get_aesthetics(data, 
                                 mapping, 
                                 variable_check = get_variable_check(envir = rastObj$canvas_env, ...),
                                 max_size = get_max_size(envir = rastObj$canvas_env, max_size = max_size), 
                                 abs_size = get_size(envir = rastObj$canvas_env, ...))
    
    start_time <- Sys.time()
    range <- get_range(x_range = xlim, 
                       y_range = ylim,
                       x = aesthetics$x, 
                       y = aesthetics$y)
    xlim <- range$x_range
    ylim <- range$y_range
    end_time <- Sys.time()
    print(paste("get range time:", end_time - start_time))
    
  } else {
    # data come from 'canvas'
    ## a new mapping system?
    aesthetics <- NULL
    if(identical(mapping, .get("mapping", envir = rastObj$canvas_env)) || rlang::is_empty(mapping)) {
      # This is encouraged, aesthetics is inherited from canvas enviroment
      if(is.null(xlim)) xlim <- .get("x_range", envir = rastObj$canvas_env)
      if(is.null(ylim)) ylim <- .get("y_range", envir = rastObj$canvas_env)
      
    } else {
      
      tryCatch(
        expr = {
          aesthetics <- get_aesthetics(data = .get("data", envir = rastObj$canvas_env), 
                                       mapping = mapping,
                                       max_size = get_max_size(envir = rastObj$canvas_env, max_size = max_size), 
                                       abs_size = get_size(envir = rastObj$canvas_env, ...))
          
          start_time <- Sys.time()
          range <- get_range(x_range = xlim, 
                             y_range = ylim,
                             x = aesthetics$x$value, 
                             y = aesthetics$y$value)
          xlim <- range$x_range
          ylim <- range$y_range
          end_time <- Sys.time()
          print(paste("get range time:", end_time - start_time))
        },
        error = function(e) {
          message("data is missing; consider to set `remove_data = FALSE` or ")
          message("set mapping aesthetics in `canvas` environment")
        }
      )
    }
  }
  args <- list(...)
  colour_key <- args$colour_key
  remove(data)
  
  e <- environment()
  rastObj <- c(
    rastObj, 
    list(e)
  )
  class(rastObj) <- c("aggregation", "canvas", "rasterizer")
  return(rastObj)
  invisible()
}

#' @export
rasterizer <- function(rastObj) {
  
  if(missing(rastObj) || !is.rasterizer(rastObj)) stop("No 'rasterizer' object", call. = FALSE)
  if(!is.aggregation(rastObj)) stop("No 'aggregation' layer", call. = FALSE)
  
  canvas_env <- rastObj[["canvas_env"]]
  rastObj[["canvas_env"]] <- NULL
  aggregation_env <- rastObj
  remove(rastObj)
  
  plot_width <- .get("plot_width", envir = canvas_env)
  plot_height <- .get("plot_height", envir = canvas_env)
  x_range <- .get("x_range", envir = canvas_env)
  y_range <- .get("y_range", envir = canvas_env)
  show_raster <- .get("show_raster", envir = canvas_env)
  
  # modify range
  lims <- lapply(aggregation_env, 
                 function(envir) {
                   xlim <- get("xlim", envir = envir, inherits = FALSE)
                   ylim <- get("ylim", envir = envir, inherits = FALSE)
                   # update ranges
                   x_range <<- range(x_range, xlim)
                   y_range <<- range(y_range, ylim)
                   
                   list(
                     xlim = xlim, 
                     ylim = ylim
                   )
                 })
  
  image <- NULL
  
  agg <- lapply(aggregation_env, 
                function(envir) {
                  # "aggregation" is a list of matrices modified by some specific reduction function
                  # if the length of "aggregation" is one, which means it is a single image
                  # else it should be multiple images 
                  xlim <- get("xlim", envir = envir, inherits = FALSE)
                  ylim <- get("ylim", envir = envir, inherits = FALSE)
                  aesthetics <- get("aesthetics", 
                                    envir = envir, 
                                    inherits = FALSE)
                  if(is.null(aesthetics)) aesthetics <- get("aesthetics", 
                                                            envir = canvas_env, 
                                                            inherits = FALSE)
                  
                  start_time <- Sys.time()
                  agg <- get_aggregation(
                    plot_width = plot_width, 
                    plot_height = plot_height,
                    aesthetics = aesthetics,
                    x_range = x_range, y_range = y_range, 
                    xlim = xlim, ylim = ylim,
                    func = get("reduction_func", envir = envir, inherits = FALSE),
                    glyph = get("glyph", envir = envir, inherits = FALSE),
                    group_by_data_table = get("group_by_data_table", envir = envir, inherits = FALSE)
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
                    image <<- if(is_categorical) {
                      get_raster_3D(L = agg,    
                                    colour_key = get_colour_key(
                                      colour_key = get("colour_key", 
                                                       envir = envir, 
                                                       inherits = FALSE), 
                                      n = len_agg,
                                      canvas_colour_key = get("colour_key", 
                                                              envir = canvas_env, 
                                                              inherits = FALSE)
                                    ),          
                                    layout = get("layout", envir = envir, inherits = FALSE), 
                                    span = get("span", envir = envir, inherits = FALSE),
                                    zeroIgnored = TRUE, 
                                    image = image, 
                                    background = get("background", envir = envir, inherits = FALSE),
                                    alpha = get("alpha", envir = envir, inherits = FALSE))
                    } else {
                      get_raster_2D(M = agg[[1]], 
                                    colour_map = get("colour_map", 
                                                     envir = envir, 
                                                     inherits = FALSE), 
                                    span = get("span", envir = envir, inherits = FALSE),
                                    zeroIgnored = TRUE, 
                                    image = image, 
                                    background = get("background", envir = envir, inherits = FALSE),
                                    alpha = get("alpha", envir = envir, inherits = FALSE),
                                    layout = get("layout", envir = envir, inherits = FALSE))
                    }
                    end_time <- Sys.time()
                    print(paste("get raster time:", end_time - start_time))
                  }
                  return(agg)
                }
  )

  l <- list(
    agg = agg, 
    image = image,
    lims = lims,
    x_range = x_range, 
    y_range = y_range,
    plot_height = plot_height,
    plot_width = plot_width
  )
  return(l)
}

is.aggregation <- function(x) {
  inherits(x, "aggregation")
}

.get <- function(x, envir = parent.frame(), inherits = FALSE) {
  
  m <- mget(x, envir = envir, ifnotfound = list(NULL),
            inherits = inherits)
  return(m[[x]])
}

#' @export
aes <- ggplot2::aes

#' @export
`%>%` <- magrittr::`%>%`
