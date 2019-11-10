get_range <- function(x_range, y_range, x, y) {
  
  x_range <- x_range %||% c(min(x), max(x))
  if(length(x_range) != 2 || !is.numeric(x_range)) stop("x_range should be a vector of type numeric with a length of 2.")
  if(x_range[1] > x_range[2]) x_range <- sort(x_range)
  
  
  y_range <- y_range %||% c(min(y), max(y))
  if(length(y_range) != 2 || !is.numeric(y_range)) stop("y_range should be a vector of type numeric with a length of 2.")
  if(y_range[1] > y_range[2]) y_range <- sort(y_range)
  
  return(
    list(
      x_range = x_range,
      y_range = y_range
    )
  )
}

get_background <- function(envir, ...) {
  
  args <- list(...)
  
  background <- args$background %||% .get("background", envir = envir) %||% "white"
  
  return(background)
}

get_color <- function(envir, mapping, ...) {
  
  args <- list(...)
  is_color_in_mapping <- mapping$color
  if(is.null(is_color_in_mapping)) {
    # color_map
    color <- args$color_map %||% args$color
  } else {
    # color_key
    color <- args$color_key %||% args$color
  }
  color_warning(envir, args)
  
  return(color %||% .get("color", envir = envir))
}

get_mapped_color <- function(color = c('lightblue','darkblue'),
                             span = 50) {
  
  # get color rgb value
  rgb_num <- get_rgb_num(color)
  span <- max(span, length(color))
  # use interpolation to extend color
  col_index <- interpolation(red = rgb_num$red, green = rgb_num$green, blue = rgb_num$blue,
                             span = span)
  
  col_index
}

get_alpha <- function(envir, ...) {
  
  args <- list(...)
  
  alpha <- args$alpha %||% .get("alpha", envir = envir) %||% 1
  
  stopifnot(
    exprs = {
      is.numeric(alpha)
      alpha <= 1 && alpha >= 0
    }
  )
  
  return(alpha)
}

get_span <- function(envir, ...) {
  
  args <- list(...)
  
  span <- args$span %||% .get("span", envir = envir) %||% 50
  
  stopifnot(
    exprs = {
      is.numeric(span)
    }
  )
  
  return(span)
}

get_max_size <- function(envir, max_size) {
  
  max_size <- max_size %||% .get("max_size", envir = envir) %||% 2
  
  if(!is.numeric(max_size)) stop("`max_size` is a numerical value", call. = FALSE)
  if(length(max_size) > 1) warning("More than one `max_size` was passed, but only the first element will be used.",
                                   call. = FALSE)
  max_size <- ceiling(max_size)[1]
  if(max_size < 2) {
    warning("`max_size` cannot be smaller than 2", call. = FALSE)
    max_size <- 2
  }
  
  return(max_size)
}

get_size <- function(envir, ...) {
  
  args <- list(...)
  
  size <- args$size %||% .get("size", envir = envir) %||% 1
  
  if(!is.numeric(size)) stop("`size` must be an integer.", call. = FALSE)
  if(length(size) > 1) {
    warning("More than one `size` was passed, but only the first element will be used.", call. = FALSE)
    size <- size[1]
  }
  if(size < 1) {
    warning("`size` should be larger than or equal to 1.", call. = FALSE)
    size <- 1
  }
  
  return(size)
}

get_variable_check <- function(envir, ...) {
  
  args <- list(...)
  
  variable_check <- args$variable_check %||% .get("variable_check", envir = envir)
  
  if(!is.logical(variable_check) || is.null(variable_check) || is.na(variable_check)) {
    warning("variable_check must be a logical value; assuming FALSE.")
    variable_check <- FALSE
  }
  
  return(variable_check)
}

get_layout <- function(envir, ...) {
  
  args <- list(...)
  parent_args <- .get("args", envir = envir)
  
  layout <- args$layout %||% parent_args$layout %||% "weighted"
  
  if(!layout %in% c("weighted", "cover")) {
    warning("`layout` options in this release include 'weighted' or 'cover'; assuming 'weighted'", call. = FALSE)
    layout <- "weighted"
  }
  
  return(layout)
}

get_glyph <- function(envir, glyph) {
  
  parent_args <- .get("args", envir = envir)
  glyph <- glyph %||% parent_args$glyph %||% "circle" 
  
  if(!glyph %in% c("circle", "square")) {
    warning("`glyph` options in this release include 'circle' or 'square'; assuming 'circle'", call. = FALSE)
    glyph <- "circle"
  }
  
  return(glyph)
}

get_x_pretty <- function(envir, x_pretty) {
  
  parent_args <- .get("args", envir = envir)
  return(x_pretty %||% parent_args$x_pretty)
}

get_y_pretty <- function(envir, y_pretty) {
  
  parent_args <- .get("args", envir = envir)
  return(y_pretty %||% parent_args$y_pretty)
}

get_group_by_data_table <- function(envir, group_by_data_table) {
  
  parent_args <- .get("args", envir = envir)
  group_by_data_table <- group_by_data_table %||% parent_args$group_by_data_table %||% TRUE
  
  if(!is.logical(group_by_data_table)) {
    warning("group_by_data_table must be a logical value; assuming TRUE.")
    group_by_data_table <- TRUE
  }
  
  return(group_by_data_table)
}
