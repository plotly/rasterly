#' @inherit add_rasterly
#' @param color Numeric vector or expression. Pixel color for each observation, to be passed on to \code{aes()}.
#' @rdname add_rasterly
#' @export

add_rasterly_image <- function(p,
                               x = NULL, y = NULL, z = NULL, ...,
                               data = NULL, inherit = TRUE,
                               color = NULL, on = NULL, size = NULL) {
  if (inherit) {
    x <- x %||% p$x$attrs[[1]][["x"]]
    y <- y %||% p$x$attrs[[1]][["y"]]
    z <- z %||% p$x$attrs[[1]][["z"]]
  }
  
  args <- list(...)
  rasterly_args <- c(
    union(methods::formalArgs(rasterly), methods::formalArgs(rasterly_points)),
    "color_map",
    "colour_map",
    "color_key",
    "colour_key"
  )
  args[rasterly_args] <- NULL
  
  if (is.null(z)) {
    # produce z by rasterly
    ### set vars
    data <- data %||% p$x$visdat[[1]]()
    on <- on %||% p$x$attrs[[1]][["on"]]
    size <- size %||% p$x$attrs[[1]][["size"]]
    color <- color %||% p$x$attrs[[1]][["color"]]
    
    ### set mappings
    mapping_names <- c("x", "y", "on", "size", "color")
    names(mapping_names) <- mapping_names
    mapping <- aes()
    expressions <- stats::setNames(
      list(x, y, on, size, color),
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
          stop("'size' ,'on' and 'color' are neither `quote` nor a numerical value.", call. = FALSE)
        }
      }
    }
    
    mapping <- Filter(Negate(is.null), mapping)
    names(mapping) <- stats::na.omit(mapping_names)
    
    data %>%
      rasterly(mapping = mapping,
               show_raster = TRUE,
               ...) %>%
      rasterly_points() %>%
      rasterly_build() -> rastObj
    data <- NULL
    
    z <- rastObj$image
    dimZ <- dim(z)
    remove(rastObj)
    
    do.call(
      plotly::add_image,
      c(
        list(
          p = p,
          z = z,
          x0 = rastObj$x_range[1],
          dx = diff(rastObj$x_range)/dimZ[2],
          y0 = rastObj$y_range[2],
          dy = -diff(rastObj$y_range)/dimZ[1],
          data = data,
          inherit = inherit
        ),
        args
      )
    ) %>% plotly::layout(
      xaxis = list(range = rastObj$x_range),
      yaxis = list(range = rastObj$y_range)
    )
    
  } else {
    message("If z is provided, `plotly::add_image` will be implemented.")
    do.call(
      plotly::add_image,
      c(
        list(
          p = p,
          z = z,
          data = data,
          inherit = inherit
        ),
        args
      )
    )
  }
}
