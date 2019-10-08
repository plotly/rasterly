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
  
  background <- args$background %||% .get("background", envir = envir)
  
  if(is.null(background)) stop("No background color was provided.", call. = FALSE)
  
  return(background)
}

get_color_map <- function(envir, ...) {
  
  args <- list(...)
  names_args <- names(args)
  col <- c("color", "color_map")
  
  which_col_name_is_mapped <- which(names_args %in% col)
  matched_name_len <- length(which_col_name_is_mapped)
  
  if(matched_name_len == 0) {
    
    return(.get("color_map", envir = envir))
    
  } else if(matched_name_len > 1) {
    
    col_name <- col[which(col %in% names_args)[1]]
    return(args[[col_name]])
    
  } else return(args[[which_col_name_is_mapped]])
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

get_color_key <- function(color_key, n, rasterly_color_key) {
  
  color_key <- color_key %||% rasterly_color_key %||% gg_color_hue(n)
  
  stopifnot(
    exprs = {
      length(color_key) >= n
    }
  )
  return(color_key)
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
  layout <- args$layout %||% .get("layout", envir = envir) %||% "weighted"
  
  if(!layout %in% c("weighted", "cover")) {
    warning("`layout` options in this release include 'weighted' or 'cover'; assuming 'weighted'", call. = FALSE)
    layout <- "weighted"
  }
  
  return(layout)
}

get_glyph <- function(envir, glyph) {
  
  glyph <- glyph %||% .get("glyph", envir = envir) %||% "circle" 
  
  if(!glyph %in% c("circle", "square")) {
    warning("`glyph` options in this release include 'circle' or 'square'; assuming 'circle'", call. = FALSE)
    glyph <- "circle"
  }
  
  return(glyph)
}

get_group_by_data_table <- function(envir, group_by_data_table) {
  
  group_by_data_table <- group_by_data_table %||% .get("group_by_data_table", envir = envir) %||% TRUE
  
  if(!is.logical(group_by_data_table)) {
    warning("group_by_data_table must be a logical value; assuming TRUE.")
    group_by_data_table <- TRUE
  }
  
  return(group_by_data_table)
}
