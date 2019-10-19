#' @title Add "rasterly" trace to a Plotly visualization
#' @description Add trace to a Plotly visualization.
#' @param p A \code{plotly} object
#' @param x Numeric vector or expression. The x variable, to be passed on to `aes()`.
#' @param y Numeric or expression. The y variable, to be passed on to `aes()`.
#' @param z Numeric. A numeric matrix (optional), to be processed with `add_heatmap`.
#' @param data A data.frame or \link[crosstalk]{SharedData} object (optional).
#' @param inherit Logical. Inherit attributes from \link[plotly]{plotly}?
#' @param on Numeric vector or expression. Provides the data on which to reduce, to be passed on to `aes()`.
#' @param size Numeric vector or expression. Pixel size for each observation, to be passed on to `aes()`.
#' @param scaling Character string or function. The scaling method to be used for the trace.
#' @param ... Arguments (i.e., attributes) passed along to the trace type or `rasterly`.
#' @export
#'
#' @examples
#' \dontrun{
#'library(rasterly)
#'if(requireNamespace("plotly") && requireNamespace("data.table")) {
#'  # Load data
#'  url1 <- "https://raw.githubusercontent.com/plotly/datasets/master/uber-rides-data1.csv"
#'  ridesRaw_1 <-  url1 %>%
#'    data.table::fread(stringsAsFactors = FALSE)
#'  url2 <- "https://raw.githubusercontent.com/plotly/datasets/master/uber-rides-data2.csv"
#'  ridesRaw_2 <-  url2 %>%
#'    data.table::fread(stringsAsFactors = FALSE)
#'  url3 <- "https://raw.githubusercontent.com/plotly/datasets/master/uber-rides-data3.csv"
#'  ridesRaw_3 <-  url3 %>%
#'    data.table::fread(stringsAsFactors = FALSE) 
#'    
#'  ridesDf <- list(ridesRaw_1, ridesRaw_2, ridesRaw_3) %>%
#'    data.table::rbindlist()
#'
#'  #### quick start
#'  p <- plot_ly(data = ridesDf) %>%
#'         add_rasterly_heatmap(x = ~Lat, y = ~Lon)
#'  p
#'  #### set artificial scaling function
#'  zeroOneTransform <- function(z) {
#'    minz <- min(z)
#'    maxz <- max(z)
#'    M <- matrix((z - minz)/(maxz - minz), nrow = dim(z)[1])
#'    return(M)
#'  }
#'  plot_ly(data = ridesDf) %>%
#'    add_rasterly_heatmap(x = ~Lat,
#'                 y = ~Lon,
#'                 on = ~-Lat,
#'                 reduction_func = "max",
#'                 scaling = zeroOneTransform) %>%
#'    plotly::layout(
#'      xaxis = list(
#'        title = "x"
#'      ),
#'      yaxis = list(
#'        title = "y"
#'      )
#'    )
#'  }
#' }
add_rasterly_heatmap <- function(p,
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
  rasterly_args <- union(methods::formalArgs(rasterly), methods::formalArgs(rasterize_points))
  args[rasterly_args] <- NULL

  if (is.null(z)) {
    # produce z by rasterly
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
        if(rlang::is_formula(exp)) {
          the_parse <-  sub("~", "", rlang::expr_text(exp)) %>%
            rlang::parse_expr()
          mapping[[i]] <- rlang::quo(!!the_parse)
        } else if(is.numeric(exp)) {
            data[[mapping_names[i]]] <- exp
            mapping[[i]] <- rlang::quo(!!rlang::parse_expr(mapping_names[i]))
        } else {
          stop("'size' ,'on' are neither `quote` nor a numerical value.", call. = FALSE)
        }
      }
    }

    mapping <- Filter(Negate(is.null), mapping)
    names(mapping) <- stats::na.omit(mapping_names)

    data %>%
      rasterly(mapping = mapping,
                 show_raster = FALSE,
                 ...) %>%
      rasterize_points() %>%
      rasterly_build() -> rastObj
    remove(data)
    data <- NULL

    if(sum(lengths(rastObj$agg)) > 1)
      message("More than one aggregation matrix was detected.")

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
      if(!is.character(scaling)) stop("'scaling' must either be an R function or a character string.")
      switch(scaling,
             "log" = {
               z <- matrix(log(z + 1), nrow = dimZ[1])
             },
             "origin" = NULL)
    }
  } else message("If z is provided, `plotly::add_heatmap` will be called.")

  do.call(
    add_trace_classed,
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
