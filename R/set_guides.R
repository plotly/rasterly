set_guides <- function(x, 
                       x_range = NULL,
                       y_range = NULL,
                       x_pretty = NULL, 
                       y_pretty = NULL, 
                       panel_background ="grey92",
                       panel_line = "white",
                       background = "white") {
  if(missing(x)) stop("Missing x")
  UseMethod("set_guides", x)
}

set_guides.matrix <- function(x, 
                              x_range = NULL,
                              y_range = NULL,
                              x_pretty = NULL, 
                              y_pretty = NULL, 
                              panel_background ="grey92",
                              panel_line = "white",
                              background = "white") {
  image <- as.raster(x)
  set_image_guides(image = image, 
                   x_range = x_range,
                   y_range = y_range,
                   x_pretty = x_pretty, 
                   y_pretty = y_pretty,
                   panel_background = panel_background,
                   panel_line = panel_line,
                   background = background)
}

set_guides.raster <- function(x, 
                              x_range = NULL,
                              y_range = NULL,
                              x_pretty = NULL, 
                              y_pretty = NULL, 
                              panel_background ="grey92",
                              panel_line = "white",
                              background = "white") {
  set_image_guides(image = x, 
                   x_range = x_range,
                   y_range = y_range,
                   x_pretty = x_pretty, 
                   y_pretty = y_pretty,
                   panel_background = panel_background,
                   panel_line = panel_line,
                   background = background)
}

set_guides.rasterly <- function(x, 
                                x_range = NULL,
                                y_range = NULL,
                                x_pretty = NULL, 
                                y_pretty = NULL,
                                panel_background ="grey92",
                                panel_line = "white",
                                background = "white") {
  
  image <- x$image
  set_image_guides(image = image, 
                   x_range = x_range,
                   y_range = y_range,
                   x_pretty = x_pretty, 
                   y_pretty = y_pretty,
                   panel_background = panel_background,
                   panel_line = panel_line,
                   background = background)
}

set_image_guides <- function(image, 
                             x_range = NULL,
                             y_range = NULL,
                             x_pretty = NULL, 
                             y_pretty = NULL, 
                             panel_background ="grey92",
                             panel_line = "white",
                             background = "white") {
  UseMethod("set_image_guides", x_range)
}

set_image_guides.default <- function(image, 
                                     x_range = NULL,
                                     y_range = NULL,
                                     x_pretty = NULL, 
                                     y_pretty = NULL, 
                                     panel_background ="grey92",
                                     panel_line = "white",
                                     background = "white") {
  
  if(missing(image)) stop("No image")
  if(is.list(y_range)) stop("`x_range` is a vector but `y_range` is a list")
  if(length(background) > 1) {
    warning("More than one background is set.\n
            We only take the first one")
    background <- background[1]
  }
  
  dim_image <- dim(image)
  
  x_range <- x_range %||% c(0, dim_image[2])
  y_range <- y_range %||% c(0, dim_image[1])
  
  stopifnot(
    exprs = {
      length(x_range) == 2
      length(y_range) == 2
    }
  )
  
  x_pretty <- x_pretty %||% grid::grid.pretty(x_range)
  y_pretty <- y_pretty %||% grid::grid.pretty(y_range)
  
  xx <- round(((x_pretty - x_range[1])/diff(x_range)) * (dim_image[2] - 1) + 1)
  yy <- round(((y_pretty - y_range[1])/diff(y_range)) * (dim_image[1] - 1) + 1)
  
  image[image == background] <- panel_background
  image <- matrix(image, nrow = dim_image[1])
  
  for(x_pos in xx) {
    column <- image[, x_pos]
    column[column == panel_background] <- panel_line
    image[, x_pos] <- column
  }
  
  for(y_pos in yy) {
    row <- image[y_pos, ]
    row[row == panel_background] <- panel_line
    image[y_pos, ] <- row
  }
  
  return(t(image))
}

set_image_guides.list <- function(image, 
                                  x_range,
                                  y_range,
                                  x_pretty = NULL, 
                                  y_pretty = NULL, 
                                  panel_background ="grey92",
                                  panel_line = "white",
                                  background = "white") {
  
  xlim <- range(unlist(x_range))
  ylim <- range(unlist(y_range))
  
  if(length(unique(background)) == 1) {
    
    x_pretty <- unlist(x_pretty)
    y_pretty <- unlist(y_pretty)
    
    image <- set_image_guides(image, 
                              xlim,
                              ylim,
                              x_pretty, 
                              y_pretty, 
                              panel_background,
                              panel_line,
                              background = background[1])
    return(image)
  }
  
  if(missing(image)) stop("No image")
  if(!is.list(y_range)) stop("`x_range` is a list but `y_range` is not")
  
  len_x_range <- length(x_range)
  if(len_x_range != length(y_range)) stop("The lengths of `x_range` is not equal to the length of `y_range`.")
  
  if(!is.null(x_pretty)) {
    if(!is.list(x_pretty)) {
      warning("`x_pretty` is not a list but `x_range` is")
      x_pretty <- NULL
    } else {
      if(length(x_pretty) != len_x_range) {
        warning("The length of `x_pretty` is not equal to the length of `x_range`.")
        x_pretty <- NULL
      }
    }
  }
  
  if(!is.null(y_pretty)) {
    if(!is.list(y_pretty)) {
      warning("`y_pretty` is not a list but `y_range` is")
      y_pretty <- NULL
    } else {
      if(length(y_pretty) != len_x_range) {
        warning("The length of `y_pretty` is not equal to the length of `y_range`.")
        y_pretty <- NULL
      }
    }
  }
  
  if(length(background) != len_x_range) {
    warning("The length of background does not match the length of `x_range`")
    background <- rep_len(background, length.out = len_x_range)
  }
  
  dim_image <- dim(image)
  lapply(1:len_x_range,
         function(i) {
           x_limit <- x_range[[i]]
           y_limit <- y_range[[i]]
           
           stopifnot(
             exprs = {
               length(x_limit) == 2
               length(y_limit) == 2
             }
           )
           
           xPretty <- x_pretty[[i]] %||% grid::grid.pretty(x_limit)
           yPretty <- y_pretty[[i]] %||% grid::grid.pretty(y_limit)
           
           xx <- round(((xPretty - x_limit[1])/diff(xlim)) * (dim_image[2] - 1) + 1)
           yy <- round(((yPretty - y_limit[1])/diff(ylim)) * (dim_image[1] - 1) + 1)
           
           image[image == background[i]] <- panel_background
           image <- matrix(image, nrow = dim_image[1])
           
           for(x_pos in xx) {
             column <- image[, x_pos]
             column[column == panel_background] <- panel_line
             image[, x_pos] <- column
           }
           
           for(y_pos in yy) {
             row <- image[y_pos, ]
             row[row == panel_background] <- panel_line
             image[y_pos, ] <- row
           }
           
           image <<- t(image)
         })
  
  return(image)
}