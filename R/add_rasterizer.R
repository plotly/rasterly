#' @title Add "rasterizer" trace to a plotly visualization
#' @description Add trace to a plotly visualization
#' @param x The x variable, numerical vectors or expression and will be passed on `aes()`.
#' @param y The y variable, numerical vectors or expression and will be passed on `aes()`.
#' @param z A numeric matrix, if provided, `add_heatmap` will be called.
#' @param data A data frame (optional) or \link[crosstalk]{SharedData} object.
#' @param inherit Inherit attributes from \link[plotly]{plotly}
#' @param on Reduction "on" which variable, it is a numerical vectors or expression and will be passed on `aes()`.
#' @param size The size of pixel for each observation, a numerical vectors or expression and will be passed on `aes()`.
#' @param scaling It could be an artificial function or a scaling way ("log", "origin") 
#' @param ... Arguments (i.e., attributes) passed along to the trace type or `rasterizer`.
#' @export
#' 
#' @examples 
#' \dontrun{
#'    library(rasterizer)
#'    if(requireNamespace("plotly") && requireNamespace("data.table")) {
#'      # Load data
#'      ridesRaw_1 <- "https://raw.githubusercontent.com/plotly/datasets/master/uber-rides-data1.csv" %>%
#'        data.table::fread(stringsAsFactors = FALSE)
#'      ridesRaw_2 <- "https://raw.githubusercontent.com/plotly/datasets/master/uber-rides-data2.csv" %>% 
#'        data.table::fread(stringsAsFactors = FALSE)
#'      ridesRaw_3 <- "https://raw.githubusercontent.com/plotly/datasets/master/uber-rides-data3.csv"  %>% 
#'        data.table::fread(stringsAsFactors = FALSE)
#'      ridesDf <- list(ridesRaw_1, ridesRaw_2, ridesRaw_3) %>% 
#'        data.table::rbindlist()
#'        
#'      #### quick start
#'      p <- plot_ly(data = ridesDf) %>% 
#'             add_rasterizer(x = ~Lat, y = ~Lon)
#'      p
#'      #### set artificial scaling function
#'      zeroOneTransform <- function(z) {
#'        minz <- min(z)
#'        maxz <- max(z)
#'        M <- matrix((z - minz)/(maxz - minz), nrow = dim(z)[1])
#'        return(M)
#'      }
#'      plot_ly(data = ridesDf) %>% 
#'        add_rasterizer(x = ~Lat, 
#'                       y = ~Lon, 
#'                       on = ~-Lat,
#'                       reduction_func = "max",
#'                       scaling = zeroOneTransform) %>%
#'        plotly::layout(
#'          xaxis = list(
#'            title = "x"
#'          ),
#'          yaxis = list(
#'            title = "y"
#'          )
#'        )
#'    }
#' }
add_rasterizer <- function(p, 
                           x = NULL, y = NULL, z = NULL, ..., 
                           data = NULL, inherit = TRUE, 
                           on = NULL, size = NULL, 
                           scaling = NULL) {
  if (inherit) {
    x <- x %||% p$x$attrs[[1]][["x"]]
    y <- y %||% p$x$attrs[[1]][["y"]]
    z <- z %||% p$x$attrs[[1]][["z"]]
  }

  args <- list(...)
  rasterizer_args <- union(methods::formalArgs(rasterizer), methods::formalArgs(rasterize_points))
  args[rasterizer_args] <- NULL
  
  if (is.null(z)) {
    # produce z by rasterizer
    ### set vars
    data <- data %||% p$x$visdat[[1]]()
    on <- on %||% p$x$attrs[[1]][["on"]]
    size <- size %||% p$x$attrs[[1]][["size"]] 
    
    ### set mappings
    mapping_names <- c("x", "y", "on", "size")
    names(mapping_names) <- mapping_names
    mapping <- aes()
    expressions <- stats::setNames(
      list(x, y, on, size),
      mapping_names
    )

    for(i in 1:length(mapping_names)) {
      exp <- expressions[[i]]
      
      if(is.null(exp)) {
        mapping_names[i] <- NA
      } else {
        if(rlang::is_expression(exp)) {
          the_parse <- rlang::expr_text(exp) %>% 
            sub("~", "", .) %>%
            rlang::parse_expr()
          mapping[[i]] <- rlang::quo(!!the_parse)
        } else if(is.numeric(exp)) {
            data[[mapping_names[i]]] <- exp
            mapping[[i]] <- rlang::quo(!!rlang::parse_expr(mapping_names[i]))
        } else stop("'on' is neither `quote` nor a numerical value", call. = FALSE)
      }
    }

    mapping <- Filter(Negate(is.null), mapping)
    names(mapping) <- stats::na.omit(mapping_names)
    
    data %>% 
      rasterizer(mapping = mapping, 
                 show_raster = FALSE, 
                 ...) %>% 
      rasterize_points() %>%
      rasterizer_build() -> rastObj
    remove(data)
    data <- NULL
    
    if(sum(lengths(rastObj$agg)) > 1) 
      message("More than one aggregation matrix is detected")
    
    z <- rastObj$agg[[1]][[1]]
    dimZ <- dim(z)
    y <- seq(rastObj$y_range[1], rastObj$y_range[2], length.out = dimZ[1])
    x <- seq(rastObj$x_range[1], rastObj$x_range[2], length.out = dimZ[2])
    remove(rastObj)

    scaling <- scaling %||% {
      message("The default scaling is 'log'.")
      "log"
    }
    if(is.function(scaling)) {
      z <- do.call(scaling, 
                   list(z = z))
    } else {
      if(!is.character(scaling)) stop("'scaling' can be either 'function' or 'character'")
      switch(scaling, 
             "log" = {
               z <- matrix(log(z + 1), nrow = dimZ[1])
             }, 
             "origin" = NULL)
    }
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
        data = data, 
        inherit = inherit
      ),
      args
    )
  )
}