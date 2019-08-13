get_range <- function(x_range, y_range, x, y) {
  if(is.null(x_range)) {
    x_range <- c(min(x), max(x))
  } else {
    if(length(x_range) != 2 || !is.numeric(x_range)) stop("x_range should be a length 2 numerical vector")
    if(x_range[1] > x_range[2]) x_range <- sort(x_range)
  }
  
  if(is.null(y_range)) {
    y_range <- c(min(y), max(y))
  } else {
    if(length(y_range) != 2 || !is.numeric(y_range)) stop("y_range should be a length 2 numerical vector")
    if(y_range[1] > y_range[2]) y_range <- sort(y_range)
  }
  
  list(
    x_range = x_range,
    y_range = y_range
  )
}

get_background <- function(p, ...) {
  
  args <- list(...)
  
  background <- if(!is.null(args$background)) {
    args$background
  } else {
    p$background
  }
  
  if(is.null(background)) stop("No background colour", call. = FALSE)
  
  background
}

get_colour_map <- function(p, ...) {
  
  args <- list(...)
  names_args <- names(args)
  col <- c("colour", "color", "color_map", "colour_map")
  
  which_col_name_is_mapped <- which(names_args %in% col)
  matched_name_len <- length(which_col_name_is_mapped)
  
  if(matched_name_len == 0) {
    
    p$colour_map
    
  } else if(matched_name_len > 1) {
    
    col_name <- col[which(col %in% names_args)[1]]
    args[[col_name]]
    
  } else args[[which_col_name_is_mapped]]
}

get_alpha <- function(p, ...) {
  
  args <- list(...)
  
  alpha <- if(!is.null(args$alpha)) {
    args$alpha
  } else {
    if(!is.null(p$alpha)) {
      p$alpha
    } else 255
  }
  
  stopifnot(
    exprs = {
      is.numeric(alpha)
      alpha <= 255 && alpha >= 0
    }
  )
  
  alpha
}

get_span <- function(p, ...) {
  
  args <- list(...)
  
  span <- if(!is.null(args$span)) {
    args$span
  } else {
    
    if(!is.null(p$span)) {
      p$span
    } else 50
  }
  
  stopifnot(
    exprs = {
      is.numeric(span)
    }
  )
  
  span
}

get_colour_key <- function(colour_key, n, canvas_colour_key) {
  
  if(is.null(colour_key)) {
    colour_key <- canvas_colour_key
  }
  
  if(is.null(colour_key)) colour_key <- gg_color_hue(n)
  stopifnot(
    exprs = {
      length(colour_key) >= n
    }
  )
  colour_key
}

get_extend_value <- function(p, extend_value) {
  
  extend_value <- if(!is.null(extend_value)) {
    extend_value
  } else {
    p$extend_value
  }
  
  if(is.null(extend_value) || is.na(extend_value)) extend_value <- 2 else {
    if(!is.numeric(extend_value)) stop("`extend_value` is a numerical value", call. = FALSE)
    if(length(extend_value) > 1) warning("More than one `extend_value` and only the first one will be used",
                                         call. = FALSE)
    extend_value <- ceiling(extend_value)[1]
    if(extend_value < 1) {
      warning("`extend_value` cannot be smaller than 1", call. = FALSE)
      extend_value <- 1
    }
  }
  
  extend_value
}

get_size <- function(p, ...) {
  
  args <- list(...)
  
  size <- if(!is.null(args$size)) {
    args$size
  } else {
    p$size
  }
  
  size
}

get_layout <- function(p, ...) {
  
  args <- list(...)
  
  layout <- if(!is.null(args$layout)) {
    args$layout
  } else {
    p$layout
  }
  
  if(is.null(layout) || is.na(layout)) layout <- "weighted"
  layout
}

get_glyph <- function(p, glyph) {
  
  glyph <- if(!is.null(glyph)) {
    glyph
  } else {
    p$glyph
  }
  
  if(is.null(glyph) || is.na(glyph)) glyph <- "circle"
  if(!glyph %in% c("circle", "square")) {
    warning("`glyph` can only be circle or square so far")
    glyph <- "circle"
  }
  glyph
}
