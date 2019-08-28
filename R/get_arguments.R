get_range <- function(x_range, y_range, x, y) {
  
  
  x_range <- x_range %||% c(min(x), max(x))
  if(length(x_range) != 2 || !is.numeric(x_range)) stop("x_range should be a length 2 numerical vector")
  if(x_range[1] > x_range[2]) x_range <- sort(x_range)
  
  
  y_range <- y_range %||% c(min(y), max(y))
  if(length(y_range) != 2 || !is.numeric(y_range)) stop("y_range should be a length 2 numerical vector")
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
  
  background <- args$background %||% .get("background", envir = envir)
  
  if(is.null(background)) stop("No background colour", call. = FALSE)
  
  return(background)
}

get_colour_map <- function(envir, ...) {
  
  args <- list(...)
  names_args <- names(args)
  col <- c("colour", "colour", "colour_map", "colour_map")
  
  which_col_name_is_mapped <- which(names_args %in% col)
  matched_name_len <- length(which_col_name_is_mapped)
  
  if(matched_name_len == 0) {
    
    return(.get("colour_map", envir = envir))
    
  } else if(matched_name_len > 1) {
    
    col_name <- col[which(col %in% names_args)[1]]
    return(args[[col_name]])
    
  } else return(args[[which_col_name_is_mapped]])
}

get_alpha <- function(envir, ...) {
  
  args <- list(...)
  
  alpha <- args$alpha %||% {
    if(!is.null(.get("alpha", envir = envir))) {
      .get("alpha", envir = envir)
    } else 255
  }
  
  stopifnot(
    exprs = {
      is.numeric(alpha)
      alpha <= 255 && alpha >= 0
    }
  )
  
  return(alpha)
}

get_span <- function(envir, ...) {
  
  args <- list(...)
  
  span <- args$span %||% {
    if(!is.null(.get("span", envir = envir))) {
      .get("span", envir = envir)
    } else 50
  }
  
  stopifnot(
    exprs = {
      is.numeric(span)
    }
  )
  
  return(span)
}

get_colour_key <- function(colour_key, n, rasterizer_colour_key) {
  
  colour_key <- colour_key %||% rasterizer_colour_key
  
  if(is.null(colour_key)) colour_key <- gg_colour_hue(n)
  stopifnot(
    exprs = {
      length(colour_key) >= n
    }
  )
  return(colour_key)
}

get_max_size <- function(envir, max_size) {
  
  max_size <- max_size %||% .get("max_size", envir = envir)
  
  if(is.null(max_size) || is.na(max_size)) {
    max_size <- 2 
  } else {
    if(!is.numeric(max_size)) stop("`max_size` is a numerical value", call. = FALSE)
    if(length(max_size) > 1) warning("More than one `max_size` and only the first one will be used",
                                     call. = FALSE)
    max_size <- ceiling(max_size)[1]
    if(max_size < 2) {
      warning("`max_size` cannot be smaller than 2", call. = FALSE)
      max_size <- 2
    }
  }
  
  return(max_size)
}

get_size <- function(envir, ...) {
  
  args <- list(...)
  
  size <- args$size %||% .get("size", envir = envir)
  
  if(is.null(size) || is.na(size)) {
    size <- 1 
  } else {
    if(!is.numeric(size)) stop("`Size` must be integer", call. = FALSE)
    if(length(size) > 1) {
      warning("Only the first one will be used as size", call. = FALSE)
      size <- size[1]
    }
    if(size < 1) {
      warning("`Size` should be larger or equal to 1", call. = FALSE)
      size <- 1
    }
  }
  
  return(size)
}

get_variable_check <- function(envir, ...) {
  
  args <- list(...)
  
  variable_check <- args$variable_check %||% .get("variable_check", envir = envir)
  
  if(!is.logical(variable_check) || is.null(variable_check) || is.na(variable_check)) {
    warning("variable_check is logical")
    variable_check <- FALSE
  }
  
  return(variable_check)
}

get_layout <- function(envir, ...) {
  
  args <- list(...)
  
  layout <- args$layout %||%  .get("layout", envir = envir)
  
  if(is.null(layout) || is.na(layout)) {
    layout <- "weighted" 
  } else {
    if(!layout %in% c("weighted", "cover")) {
      warning("`layout` can only be 'weighted' or 'cover' so far", call. = FALSE)
      layout <- "weighted"
    }
  }
  
  return(layout)
}

get_glyph <- function(envir, glyph) {
  
  glyph <- glyph %||% .get("glyph", envir = envir)
  
  if(is.null(glyph) || is.na(glyph)) {
    glyph <- "circle" 
  } else {
    if(!glyph %in% c("circle", "square")) {
      warning("`glyph` can only be 'circle' or 'square' so far", call. = FALSE)
      glyph <- "circle"
    }
  }
  
  return(glyph)
}

get_group_by_data_table <- function(envir, group_by_data_table) {
  
  group_by_data_table <- group_by_data_table %||% .get("group_by_data_table", envir = envir)
  
  if(is.null(group_by_data_table) || is.na(group_by_data_table)) {
    group_by_data_table <- TRUE
  } else {
    if(!is.logical(group_by_data_table)) {
      group_by_data_table <- TRUE
      warning("`group_by_data_table` is logical", call. = FALSE)
    }
  }
  
  return(group_by_data_table)
}