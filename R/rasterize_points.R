#' @title rasterize_points
#' @description Points layer for "rasterizer".
#' @param rastObj A "rasterizer" object. 
#' @param data Dataset to use for plot. If not provided (say `NULL`), data would be inherited by `rasterizer()`; else 
#' input data could be a `data.frame` or some function with argument `x`.
#' @param mapping Default list of aesthetic mappings to use for plot. If provided and `inherit.aes = TRUE`, it will be
#' merged on top of `rasterizer()` mapping.
#' @param ... Arguments passed by `rasterizer()`
#' @param xlim X limits in this layer
#' @param ylim Y limits in this layer
#' @param max_size When size is modifed, how many pixels an observation point will be spreaded.
#' @param reduction_func A reduction operator function is used when aggregating datapoints
#' into a given pixel. The supported reduction operators are `sum`, `any`, `mean`, `m2`, `first`, 
#' `last`, `min` and `max` so far. Default is `sum`. See details.
#' @param layout The way to layout multiple images, default is `weighted`. 
#' It is useful when data is catergorical("colour" is set in `aes()`). The default setting is "weighted", 
#' which means the final raster is the weighted combination of each categorical aggregation matrix. 
#' Also, we can "cover" each raster by the order of categories.
#' @param glyph When increase the size, the pixels would be spreaded as a square (same speed on four directions) 
#' or a circle (It is more like diamond? Will fix in the future)
#' @param group_by_data_table Logical Value and defualt is `TRUE`. When set "colour" in `aes()`, 
#' "group by" data set by "data.table" (`TRUE`) or a Rcpp loop (`FALSE`). In general, set `group_by_data_table = TRUE` 
#' is faster, however, if the dataset is extremely large, the speed is not as stable as a Rcpp loop.
#' 
#' @seealso \link{rasterizer}, \link{rasterizer_build}, \link{[.rasterizer}, \link{[<-.rasterizer}
#' 
#' @details
#' Reduction functions
#' \itemize{
#'  \item{`sum`: If `on` is not provided in mapping `aes()`, it is the count of each bins; else it would be summation of 
#'  `on` variable falling in that bin}
#'  \item{`any`: If `on` is not provided in mapping `aes()`, it is the logical value if the bin is visited; else it would be 
#'  any `on` variable falling in that bin}
#'  \item{`mean`: If `on` is not provided in mapping `aes()`, `on` would be set as variable "y" by default. 
#'  Give the mean of `on` variable falling in that bin}
#'  \item{`m2`: `on` must be provided in mapping `aes()`. Give the sum of mean squares of `on` variable falling in that bin}
#'  \item{`var`: `on` must be provided in mapping `aes()`. Give the variance of `on` variable falling in that bin}
#'  \item{`sd`: `on` must be provided in mapping `aes()`. Give the standard deviation of `on` variable falling in that bin}
#'  \item{`first`: `on` must be provided in mapping `aes()`. Give the first `on` variable element falling in that bin}
#'  \item{`last`: `on` must be provided in mapping `aes()`. Give the last `on` variable element falling in that bin}
#'  \item{`min`: `on` must be provided in mapping `aes()`. Give the max `on` variable element falling in that bin}
#'  \item{`max`: `on` must be provided in mapping `aes()`. Give the min `on` variable element falling in that bin}
#'  
#' }
#' 
#' @return A list of environments.
#' @examples
#' \dontrun{
#'    library(rasterizer)
#'    if(requireNamespace("grid") && requireNamespace("gridExtra")) {
#'      x <- rnorm(1e7)
#'      y <- rnorm(1e7)
#'      category <- sample(1:5, 1e7, replace = TRUE)
#'      data.frame(x = x, y = y, category = category) %>%
#'        rasterizer(mapping = aes(x = x, y = y, colour = category)) %>%
#'        rasterize_points(layout = "weighted") -> ds1
#'      ds1
#'      # layout with cover
#'      data.frame(x = x, y = y, category = category) %>%
#'        rasterizer(mapping = aes(x = x, y = y, colour = category)) %>%
#'        rasterize_points(layout = "cover") -> ds2
#'      ds2
#'      # display side by side
#'      grid::grid.newpage()
#'      gridExtra::grid.arrange(
#'         grobs = list(rasterizerGrob(ds1), rasterizerGrob(ds2)),
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
  if(missing(rastObj) || !is.rasterizer(rastObj)) stop("No 'rasterizer' object", call. = FALSE)
  if(!is.rasterizer(rastObj))
    if (!missing(mapping) && !inherits(mapping, "uneval")) {
      stop("Mapping should be created with `aes()`.", call. = FALSE)
    }
  
  background <-  get_background(envir = rastObj$rasterizer_env, ...)
  colour_map <- get_colour_map(envir = rastObj$rasterizer_env, ...)
  alpha <- get_alpha(envir = rastObj$rasterizer_env, ...)
  span <- get_span(envir = rastObj$rasterizer_env, ...)
  layout <- get_layout(envir = rastObj$rasterizer_env, layout = layout)
  glyph <- get_glyph(envir = rastObj$rasterizer_env, glyph = glyph)
  group_by_data_table <- get_group_by_data_table(envir = rastObj$rasterizer_env, 
                                                 group_by_data_table = group_by_data_table)
  
  reduction_func <- if(is.null(reduction_func)) {
    func <- .get("reduction_func", envir = rastObj$rasterizer_env)
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
    
    if(is.function(data)) data <- do.call(data, list(x = .get("data", envir = rastObj$rasterizer_env)))
    
    # new input data in this layer
    if(rlang::is_empty(mapping)) {
      mapping <- .get("mapping", envir = rastObj$rasterizer_env)
    } else {
      if(inherit.aes)
        # `%<-%` is a symbol to merge two lists from right to left
        mapping <- ggplot2:::new_aes(.get("mapping", envir = rastObj$rasterizer_env) %<-% mapping)
    }
    aesthetics <- get_aesthetics(data, 
                                 mapping, 
                                 variable_check = get_variable_check(envir = rastObj$rasterizer_env, ...),
                                 max_size = get_max_size(envir = rastObj$rasterizer_env, max_size = max_size), 
                                 abs_size = get_size(envir = rastObj$rasterizer_env, ...))
    
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
    # data come from 'rasterizer'
    ## a new mapping system?
    aesthetics <- NULL
    rasterizer_env_mapping <- .get("mapping", envir = rastObj$rasterizer_env)

    if(identical(mapping, rasterizer_env_mapping) || rlang::is_empty(mapping)) {
      # This is encouraged, aesthetics is inherited from rasterizer enviroment
      if(is.null(xlim)) xlim <- .get("x_range", envir = rastObj$rasterizer_env)
      if(is.null(ylim)) ylim <- .get("y_range", envir = rastObj$rasterizer_env)
      
      mapping <- rasterizer_env_mapping
      
    } else {
      
      if(inherit.aes)
        # `%<-%` is a symbol to merge two lists from right to left
        # Should I use `new_aes()`?
        mapping <- ggplot2:::new_aes(.get("mapping", envir = rastObj$rasterizer_env) %<-% mapping)
      
      tryCatch(
        expr = {
          aesthetics <- get_aesthetics(data = .get("data", envir = rastObj$rasterizer_env), 
                                       mapping = mapping,
                                       max_size = get_max_size(envir = rastObj$rasterizer_env, max_size = max_size), 
                                       abs_size = get_size(envir = rastObj$rasterizer_env, ...))
          
          start_time <- Sys.time()
          range <- get_range(x_range = xlim, 
                             y_range = ylim,
                             x = aesthetics$x, 
                             y = aesthetics$y)
          xlim <- range$x_range
          ylim <- range$y_range
          end_time <- Sys.time()
          print(paste("get range time:", end_time - start_time))
        },
        error = function(e) {
          message("data is missing; consider to set `remove_data = FALSE` or ")
          message("set mapping aesthetics in `rasterizer` environment")
        }
      )
    }
  }
  
  
  variable_names <- stats::setNames(
    sapply(mapping, function(var) sub("~", "", rlang::expr_text(var))), 
    names(mapping)                      
  )
  
  args <- list(...)
  colour_key <- args$colour_key
  remove(data)
  
  rastObj <- c(
    rastObj, 
    setNames(
      list(environment()),
      paste0("rasterizerPoints", length(rastObj))
    )
  )
  class(rastObj) <- c("rasterizePoints", "rasterizeLayer", "rasterizer")
  return(rastObj)
  invisible()
}
