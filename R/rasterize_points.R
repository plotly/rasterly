#' @title rasterize_points
#' @description Points layer for "rasterly".
#' @param rastObj A "rasterly" object. 
#' @param data A `data.frame` or `function` with an argument `x`, specifying the dataset to use for plotting. If `data` 
#' is `NULL`, the `data` argument provided to `rasterly()` may be passed through.
#' @param mapping Default list of aesthetic mappings to use for plot. If provided and `inherit.aes = TRUE`, it will be
#' stacked on top of the mappings passed to `rasterly()`.
#' @param ... Pass-through arguments provided by `rasterly()`.
#' @param xlim Vector of type numeric. X limits in this layer.
#' @param ylim Vector of type numeric. Y limits in this layer.
#' @param max_size Numeric. When size changes, the upper bound of the number of pixels over which to spread a single observation.
#' @param reduction_func Function. A reduction function is used to aggregate data points into their pixel representations. Currently
#' supported reduction operators are `sum`, `any`, `mean`, `m2`, `first`, `last`, `min` and `max`. Default is `sum`. See details.
#' @param layout Character. The method used to generate layouts for multiple images. The default is `weighted`. Useful for categorical
#' data (i.e. "color" is provided via `aes()`). `weighted` specifies that the final raster should be a weighted combination of each
#' (categorical) aggregation matrix.
#' @param glyph Character. Currently, only "circle" and "square" are supported; as the `size` of the pixels increases, how should they
#' spread out -- should the pattern be circular or square? Other glyphs may be added in the future.
#' @param group_by_data_table Logical. Default is `TRUE`; when "color" is provided via `aes()`, the "group by" operation may be
#' perfromed within `data.table` or natively within `rasterly`. Generally, `group_by_data_table = TRUE` is faster, but for very
#' large datasets grouping within `rasterly` may offer better performance.
#' 
#' @seealso \link{rasterly}, \link{rasterly_build}, \link{[.rasterly}, \link{[<-.rasterly}
#' 
#' @details
#' Reduction functions
#' \itemize{
#'  \item{`sum`: If `on` is not provided within `aes()`, the default is to take the sum within each bin. 
#'  When `on` is specified, the function reduces by taking the sum of all elements within the variable 
#'  named in `on`.}
#'  \item{`any`: When `on` is provided within `aes()`, the `any` reduction function specifies whether any 
#'  elements in `on` should be mapped to each bin.}
#'  \item{`mean`: If `on` is not provided in mapping `aes()`, `on` would be set as variable "y" by default.
#'  When `on` is given, the `mean` reduction function takes the mean of all elements within the variable
#'  specified by `on`.}
#'  \item{`m2`: Requires that `on` is specified within `aes()`. The `m2` function computes the sum of
#'  square differences from the mean of all elements in the variable specified by `on`.}
#'  \item{`var`: Requires that `on` is specified within `aes()`. The `var` function computes the variance 
#'  over all elements in the vector specified by `on`.}
#'  \item{`sd`: Requires that `on` is specified within `aes()`. The `sd` function computes the standard 
#'  deviation over all elements in the vector specified by `on`.}
#'  \item{`first`: Requires that `on` is specified within `aes()`. The `first` function returns the first 
#'  element in the vector specified by `on`.}
#'  \item{`last`: Requires that `on` is specified within `aes()`. The `last` function returns the last
#'  element in the vector specified by `on`.}
#'  \item{`min`: Requires that `on` is specified within `aes()`. The `min` function returns the minimum
#'  value in the vector specified by `on`.}
#'  \item{`max`: Requires that `on` is specified within `aes()`. The `min` function returns the maximum 
#'  value in the vector specified by `on`.}
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
#'        rasterize_points(layout = "weighted") -> ds1
#'      ds1
#'      # layout with cover
#'      data.frame(x = x, y = y, category = category) %>%
#'        rasterly(mapping = aes(x = x, y = y, color = category)) %>%
#'        rasterize_points(layout = "cover") -> ds2
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
rasterize_points <- function(rastObj,
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
  if(!is.rasterly(rastObj))
    if (!missing(mapping) && !inherits(mapping, "uneval")) {
      stop("Mapping should be created with `aes()`.", call. = FALSE)
    }
  
  background <-  get_background(envir = rastObj$rasterly_env, ...)
  color_map <- get_color_map(envir = rastObj$rasterly_env, ...)
  alpha <- get_alpha(envir = rastObj$rasterly_env, ...)
  span <- get_span(envir = rastObj$rasterly_env, ...)
  layout <- get_layout(envir = rastObj$rasterly_env, layout = layout)
  glyph <- get_glyph(envir = rastObj$rasterly_env, glyph = glyph)
  group_by_data_table <- get_group_by_data_table(envir = rastObj$rasterly_env, 
                                                 group_by_data_table = group_by_data_table)
  
  reduction_func <- if(is.null(reduction_func)) {
    func <- .get("reduction_func", envir = rastObj$rasterly_env)
    ifelse(is.null(func), "", func)
  } else {
    if(is.character(reduction_func)) reduction_func
    else if(is.function(reduction_func)) deparse(substitute(reduction_func))
    else stop("The reduction function passed is unknown; please verify that the value of reduction_func is valid.", call. = FALSE)
  }
  if(reduction_func == "") {
    reduction_func <- "sum"
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
        # `%<-%` is a symbol to merge two lists from right to left
        mapping <- new_aes(.get("mapping", envir = rastObj$rasterly_env) %<-% mapping)
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
        mapping <- new_aes(.get("mapping", envir = rastObj$rasterly_env) %<-% mapping)
      
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
  
  args <- list(...)
  color_key <- args$color_key
  remove(data)
  
  rastObj <- c(
    rastObj, 
    setNames(
      list(environment()),
      paste0("rasterlyPoints", length(rastObj))
    )
  )
  class(rastObj) <- c("rasterlyPoints", "rasterlyLayer", "rasterly")
  invisible(return(rastObj))
}
