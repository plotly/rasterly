#' @title Add "rasterizer" trace to a plotly visualization
#' @description Add trace to a plotly visualization
#' @param x The x variable, numerical vectors or expression and will be passed on `aes()`.
#' @param y The y variable, numerical vectors or expression and will be passed on `aes()`.
#' @param z A numeric matrix, if provided, `add_heatmap` will be called.
#' @param data A data frame (optional) or \link{crosstalk::SharedData} object.
#' @param inherit Inherit attributes from \link{plot_ly()}
#' @param on Reduction "on" which variable, it is a numerical vectors or expression and will be passed on `aes()`.
#' @param size The size of pixel for each observation, a numerical vectors or expression and will be passed on `aes()`.
#' @param scaling Scale layout matrix. 
#' @param ... Arguments (i.e., attributes) passed along to the trace type or `rasterizer`.
#' @export
#' 
#' @examples 
#' \dontrun{
#'    library(rasterizer)
#'    if(requireNamespace("plotly")) {
#'      x <- rnorm(1e7)
#'      y <- rnorm(1e7)
#'      min_x <- min(x)
#'      min_y <- min(y)
#'      max_x <- max(x)
#'      max_y <- max(y)
#'      data.table::data.table(x = x, y = y) %>% 
#'        plot_ly() %>% 
#'        add_rasterizer(x = ~x, y = ~y,
#'                       x_range = c(min_x, max_x), y_range = c(min_y, max_y))
#'        
#'    }
#' }
add_rasterizer <- function(p, x = NULL, y = NULL, z = NULL, ..., 
                           data = NULL, inherit = TRUE, 
                           on = NULL, size = NULL, 
                           scaling = c("log", "to01", "origin")) {
  if (inherit) {
    x <- x %||% p$x$attrs[[1]][["x"]]
    y <- y %||% p$x$attrs[[1]][["y"]]
    z <- z %||% p$x$attrs[[1]][["z"]]
  }

  args <- list(...)
  rasterizer_args <- union(methods::formalArgs(canvas), methods::formalArgs(aggregation_points))
  args[rasterizer_args] <- NULL
  
  if (is.null(z)) {
    # produce z by rasterizer
    
    ### set names
    mapping_names <- c("x", "y", "on", "size")
    names(mapping_names) <- mapping_names
    ### set scaling
    scaling <- match.arg(scaling)
    
    data <- data %||% p$x$visdat[[1]]()
    on <- on %||% p$x$attrs[[1]][["on"]]
    size <- size %||% p$x$attrs[[1]][["size"]] 
    mapping <- aes()

    # set x
    if(rlang::is_expression(x)) {
      x_parse <- rlang::expr_text(x) %>% 
        sub("~", "", .) %>%
        rlang::parse_expr()
      mapping[[1]] <- rlang::quo(!!x_parse)
    } else {
      if(is.numeric(x)) {
        data$x <- x 
        mapping[[1]] <- rlang::quo(x)
      } else stop("'x' is neither `quote` nor a numerical value", call. = FALSE)
    }
    
    # set y
    if(rlang::is_expression(y)) {
      y_parse <- rlang::expr_text(y) %>% 
        sub("~", "", .) %>%
        rlang::parse_expr()
      mapping[[2]] <- rlang::quo(!!y_parse)
    } else {
      if(is.numeric(y)) {
        data$y <- y
        mapping[[2]] <- rlang::quo(y)
      } else stop("'y' is neither `quote` nor a numerical value", call. = FALSE)
    }
    
    # set on
    if(is.null(on)) {
      mapping_names["on"] <- NA
    } else {
      if(rlang::is_expression(on)) {
        on_parse <- rlang::expr_text(on) %>% 
          sub("~", "", .) %>%
          rlang::parse_expr()
        mapping[[3]] <- rlang::quo(!!on_parse)
      } else {
        if(is.numeric(on)) {
          data$on <- on
          mapping[[3]] <- rlang::quo(on)
        } else stop("'on' is neither `quote` nor a numerical value", call. = FALSE)
      }
    }
    
    # set size
    if(is.null(size)) {
      mapping_names["size"] <- NA
    } else {
      if(rlang::is_expression(size)) {
        size_parse <- rlang::expr_text(size) %>% 
          sub("~", "", .) %>%
          rlang::parse_expr()
        mapping[[4]] <- rlang::quo(!!size_parse)
      } else {
        if(is.numeric(size)) {
          data$size <- size
          mapping[[4]] <- rlang::quo(size)
        } else stop("'size' is neither `quote` nor a numerical value", call. = FALSE)
      }
    }
    
    mapping <- Filter(Negate(is.null), mapping)
    names(mapping) <- stats::na.omit(mapping_names)
    
    data %>% 
      canvas(mapping = mapping, 
             show_raster = FALSE, 
             ...) %>% 
      aggregation_points() %>%
      rasterizer() -> rastObj
    remove(data)
    
    if(sum(lengths(rastObj$agg)) > 1) 
      message("More than one aggregation matrix is detected")
    
    z <- rastObj$agg[[1]][[1]]
    dimZ <- dim(z)
    y <- seq(rastObj$y_range[1], rastObj$y_range[2], length.out = dimZ[1])
    x <- seq(rastObj$x_range[1], rastObj$x_range[2], length.out = dimZ[2])
    remove(rastObj)

    switch(scaling, 
           "log" = {
             z <- matrix(log(z + 1), nrow = dimZ[1])
           }, 
           "to01" = {
             z <- zeroOneTransform(z)
           }, 
           "origin" = NULL)
  } else message("If z is provided, `add_heatmap` will be excuted")
  
  do.call(
    plotly:::add_trace_classed,
    c(
      list(
        p = p, 
        class = "plotly_heatmap", 
        z = z, 
        x = x, 
        y = y,
        type = "heatmap",  
        data = NULL, 
        inherit = inherit
      ),
      args
    )
  )
}