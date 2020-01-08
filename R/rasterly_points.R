#' @title rasterly_points
#' @description Points layer for \code{rasterly}.
#' @param rastObj A \code{rasterly} object. 
#' @param data A \code{data.frame} or \code{function} with an argument \code{x}, specifying the dataset to use for plotting. If \code{data} 
#' is \code{NULL}, the \code{data} argument provided to \code{rasterly} may be passed through.
#' @param mapping Default list of aesthetic mappings to use for plot. If provided and \code{inherit.aes = TRUE}, it will be
#' stacked on top of the mappings passed to \code{rasterly}.
#' @param ... Pass-through arguments provided by \code{rasterly}.
#' @param xlim Vector of type numeric. X limits in this layer.
#' @param ylim Vector of type numeric. Y limits in this layer.
#' @param max_size Numeric. When size changes, the upper bound of the number of pixels over which to spread a single observation.
#' @param reduction_func Function. A reduction function is used to aggregate data points into their pixel representations. Currently
#' supported reduction operators are \code{sum}, \code{any}, \code{mean}, \code{m2}, \code{first}, \code{last}, \code{min} and \code{max}. Default is \code{sum}. See details.
#' @param layout Character. The method used to generate layouts for multiple images. The default is \code{weighted}. Useful for categorical
#' data (i.e. "color" is provided via \code{aes()}). \code{weighted} specifies that the final raster should be a weighted combination of each
#' (categorical) aggregation matrix. Conversely, \code{cover} indicates that the afterwards objects will be drawn on top of 
#' the previous ones.
#' @param glyph Character. Currently, only "circle" and "square" are supported; as the \code{size} of the pixels increases, how should they
#' spread out -- should the pattern be circular or square? Other glyphs may be added in the future.
#' @param group_by_data_table Logical. Default is \code{TRUE}; when "color" is provided via \code{aes()}, the "group by" operation may be
#' perfromed within \code{data.table} or natively within \code{rasterly}. Generally, \code{group_by_data_table = TRUE} is faster, but for very
#' large datasets grouping within \code{rasterly} may offer better performance.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics, rather than combining with them.
#' 
#' @seealso \link{rasterly}, \link{rasterly_build}, \link{[.rasterly}, \link{[<-.rasterly}
#' 
#' @details
#' Reduction functions
#' \itemize{
#'  \item{\code{sum}: If \code{on} is not provided within \code{aes()}, the default is to take the sum within each bin. 
#'  When \code{on} is specified, the function reduces by taking the sum of all elements within the variable 
#'  named in \code{on}.}
#'  \item{\code{any}: When \code{on} is provided within \code{aes()}, the \code{any} reduction function specifies whether any 
#'  elements in \code{on} should be mapped to each bin.}
#'  \item{\code{mean}: If \code{on} is not provided in mapping \code{aes()}, \code{on} would be set as variable "y" by default.
#'  When \code{on} is given, the \code{mean} reduction function takes the mean of all elements within the variable
#'  specified by \code{on}.}
#'  \item{\code{m2}: Requires that \code{on} is specified within \code{aes()}. The \code{m2} function computes the sum of
#'  square differences from the mean of all elements in the variable specified by \code{on}.}
#'  \item{\code{var}: Requires that \code{on} is specified within \code{aes()}. The \code{var} function computes the variance 
#'  over all elements in the vector specified by \code{on}.}
#'  \item{\code{sd}: Requires that \code{on} is specified within \code{aes()}. The \code{sd} function computes the standard 
#'  deviation over all elements in the vector specified by \code{on}.}
#'  \item{\code{first}: Requires that \code{on} is specified within \code{aes()}. The \code{first} function returns the first 
#'  element in the vector specified by \code{on}.}
#'  \item{\code{last}: Requires that \code{on} is specified within \code{aes()}. The \code{last} function returns the last
#'  element in the vector specified by \code{on}.}
#'  \item{\code{min}: Requires that \code{on} is specified within \code{aes()}. The \code{min} function returns the minimum
#'  value in the vector specified by \code{on}.}
#'  \item{\code{max}: Requires that \code{on} is specified within \code{aes()}. The \code{min} function returns the maximum 
#'  value in the vector specified by \code{on}.}
#' }
#' 
#' @return A list of environments.
#' @examples
#' \dontrun{
#'    library(rasterly)
#'    if(requireNamespace("grid") && requireNamespace("gridExtra")) {
#'      x <- rnorm(1e7)
#'      y <- rnorm(1e7)
#'      category <- sample(1:5, 1e7, replace = TRUE)
#'      data.frame(x = x, y = y, category = category) %>%
#'        rasterly(mapping = aes(x = x, y = y, color = category)) %>%
#'        rasterly_points(layout = "weighted") -> ds1
#'      ds1
#'      # layout with cover
#'      data.frame(x = x, y = y, category = category) %>%
#'        rasterly(mapping = aes(x = x, y = y, color = category)) %>%
#'        rasterly_points(layout = "cover") -> ds2
#'      ds2
#'      # display side by side
#'      grid::grid.newpage()
#'      gridExtra::grid.arrange(
#'         grobs = list(rasterlyGrob(ds1), rasterlyGrob(ds2)),
#'         ncol = 2,
#'         top = "'weighted' layout versus 'cover' layout"
#'      )
#'    }
#' }

#' @export
rasterly_points <- function(rastObj,
                            data = NULL,
                            mapping = aes(),
                            ...,
                            xlim = NULL,
                            ylim = NULL,
                            max_size = NULL,
                            reduction_func = NULL,
                            layout = NULL,
                            glyph = NULL,
                            group_by_data_table = NULL,
                            inherit.aes = TRUE) {
  
  # argument check
  if(missing(rastObj) || !is.rasterly(rastObj)) stop("No 'rasterly' object", call. = FALSE)
  mapping <- rename_mapping(mapping)
  
  background <-  get_background(envir = rastObj$rasterly_env, ...)
  span <- get_span(envir = rastObj$rasterly_env, ...)
  layout <- get_layout(envir = rastObj$rasterly_env, layout = layout)
  glyph <- get_glyph(envir = rastObj$rasterly_env, glyph = glyph)
  group_by_data_table <- get_group_by_data_table(envir = rastObj$rasterly_env, 
                                                 group_by_data_table = group_by_data_table)
  
  reduction_func <- if(is.null(reduction_func)) {
    parent_args <- .get("args", envir = rastObj$rasterly_env)
    func <- parent_args$reduction_func %||% "sum"
  } else {
    if(is.character(reduction_func)) reduction_func
    else if(is.function(reduction_func)) deparse(substitute(reduction_func))
    else stop("The reduction function passed is unknown; please verify that the value of reduction_func is valid.", call. = FALSE)
  }
  # for S3 method
  class(reduction_func) <- reduction_func
  
  if(!is.null(data)) {
    
    if(is.function(data)) data <- do.call(data, list(x = .get("data", envir = rastObj$rasterly_env)))
    
    # new input data in this layer
    if(rlang::is_empty(mapping)) {
      mapping <- .get("mapping", envir = rastObj$rasterly_env)
    } else {
      if(inherit.aes)
        # }%<-%` is a symbol to merge two lists from right to left
        mapping <- mbind(.get("mapping", envir = rastObj$rasterly_env), 
                         mapping)
    }
    aesthetics <- get_aesthetics(data, 
                                 mapping, 
                                 variable_check = get_variable_check(envir = rastObj$rasterly_env, ...),
                                 max_size = get_max_size(envir = rastObj$rasterly_env, max_size = max_size), 
                                 abs_size = get_size(envir = rastObj$rasterly_env, ...))
    
    range <- get_range(x_range = xlim, 
                       y_range = ylim,
                       x = aesthetics$x, 
                       y = aesthetics$y)
    xlim <- range$x_range
    ylim <- range$y_range
    
  } else {
    # data come from 'rasterly'
    ## a new mapping system?
    aesthetics <- NULL
    rasterly_env_mapping <- .get("mapping", envir = rastObj$rasterly_env)
    
    if(identical(mapping, rasterly_env_mapping) || rlang::is_empty(mapping)) {
      # This is encouraged, aesthetics is inherited from rasterly enviroment
      if(is.null(xlim)) xlim <- .get("x_range", envir = rastObj$rasterly_env)
      if(is.null(ylim)) ylim <- .get("y_range", envir = rastObj$rasterly_env)
      
      mapping <- rasterly_env_mapping
      
    } else {
      
      if(inherit.aes)
        # `%<-%` is a symbol to merge two lists from right to left
        mapping <- mbind(.get("mapping", envir = rastObj$rasterly_env), 
                         mapping)
      
      tryCatch(
        expr = {
          aesthetics <- get_aesthetics(data = .get("data", envir = rastObj$rasterly_env), 
                                       mapping = mapping,
                                       max_size = get_max_size(envir = rastObj$rasterly_env, max_size = max_size), 
                                       abs_size = get_size(envir = rastObj$rasterly_env, ...))
          
          range <- get_range(x_range = xlim, 
                             y_range = ylim,
                             x = aesthetics$x, 
                             y = aesthetics$y)
          xlim <- range$x_range
          ylim <- range$y_range
        },
        error = function(e) {
          message("data is missing; consider setting `remove_data = FALSE` or ")
          message("provide mapping aesthetics in `rasterly` environment")
        }
      )
    }
  }
  
  variable_names <- stats::setNames(
    sapply(mapping, function(var) sub("~", "", rlang::expr_text(var))), 
    names(mapping)                      
  )
  
  color <- get_color(envir = rastObj$rasterly_env, mapping, ...)
  remove(data)
  
  p <- structure(
    setNames(
      list(environment()),
      paste0("rasterlyPoints", length(rastObj))
    ),
    class = c("rasterlyPoints", "rasterlyLayer")
  )
  rastObj <- c(
    rastObj, 
    p
  )
  class(rastObj) <- c("rasterly")
  invisible(return(rastObj))
}
