#' @title aggregation_points
#' @description points layer for `rasterizer`
#' @param rastObj A `rasterObj`. 
#' @param reduction_func A reduction operator function is used when aggregating datapoints
#' into a given pixel. The supported reduction operators are `sum`, `any`, `mean`, `first` and `last` so far.
#' More is coming
#' @param layout The way to layout multiple images. It is useful when data is catergorical("colour" is set in `aes()`).
#' The default setting is "weighted", which means the final raster is the weighted combination of each category
#' aggregation matrix. Also, we can "cover" each raster by the order of unique categories.

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
  if(!is.rasterizer(rastObj))
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
