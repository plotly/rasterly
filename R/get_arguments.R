get_colour_map <- function(p, ...) {

  args <- list(...)
  names_args <- names(args)
  col <- c("colour", "color", "color_map", "colour_map")

  which_col_name_is_mapped <- which(names_args %in% col)
  matched_name_len <- length(which_col_name_is_mapped)

  if(matched_name_len == 0) {

    if(missing(p)) stop("'rasterizer' is missing")
    if(!is.rasterizer(p)) stop(paste(deparse(substitute(p)), "is not a 'rasterizer' object"))
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

    if(missing(p)) stop("'rasterizer' is missing")
    if(!is.rasterizer(p)) stop(paste(deparse(substitute(p)), "is not a 'rasterizer' object"))

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

    if(missing(p)) stop("'rasterizer' is missing")
    if(!is.rasterizer(p)) stop(paste(deparse(substitute(p)), "is not a 'rasterizer' object"))

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

get_colour_key <- function(p, n, ...) {

  args <- list(...)

  colour_key <- if(!is.null(args$colour_key)) {
    args$colour_key
  } else {

    if(missing(p)) stop("'rasterizer' is missing")
    if(!is.rasterizer(p)) stop(paste(deparse(substitute(p)), "is not a 'rasterizer' object"))

    p$colour_key
  }

  if(is.null(colour_key)) colour_key <- gg_color_hue(n)
  stopifnot(
    exprs = {
      length(colour_key) >= n
    }
  )
  colour_key
}

get_extend_rate <- function(p, extend_rate) {

  extend_rate <- if(!is.null(extend_rate)) {
    extend_rate
  } else {

    if(missing(p)) stop("'rasterizer' is missing")
    if(!is.rasterizer(p)) stop(paste(deparse(substitute(p)), "is not a 'rasterizer' object"))

    p$extend_rate
  }

  if(is.null(extend_rate) || is.na(extend_rate)) extend_rate <- 2 else {
    if(!is.numeric(extend_rate)) stop("`extend_rate` is a numerical value", call. = FALSE)
    if(length(extend_rate) > 1) warning("More than one `extend_rate` and only the first one will be used",
                                      call. = FALSE)
    extend_rate <- ceiling(extend_rate)[1]
    if(extend_rate < 1) {
      warning("`extend_rate` cannot be smaller than 1", call. = FALSE)
      extend_rate <- 1
    }
  }

  extend_rate
}

get_size <- function(p, ...) {

  args <- list(...)

  size <- if(!is.null(args$size)) {
    args$size
  } else {

    if(missing(p)) stop("'rasterizer' is missing")
    if(!is.rasterizer(p)) stop(paste(deparse(substitute(p)), "is not a 'rasterizer' object"))

    p$size
  }

  size
}

get_layout <- function(p, ...) {

  args <- list(...)

  layout <- if(!is.null(args$layout)) {
    args$layout
  } else {

    if(missing(p)) stop("'rasterizer' is missing")
    if(!is.rasterizer(p)) stop(paste(deparse(substitute(p)), "is not a 'rasterizer' object"))

    p$layout
  }

  if(is.null(layout) || is.na(layout)) layout <- "weighted"
  layout
}

get_glyph <- function(p, glyph) {

  glyph <- if(!is.null(glyph)) {
    glyph
  } else {

    if(missing(p)) stop("'rasterizer' is missing")
    if(!is.rasterizer(p)) stop(paste(deparse(substitute(p)), "is not a 'rasterizer' object"))

    p$glyph
  }

  if(is.null(glyph) || is.na(glyph)) glyph <- "circle"
  if(!glyph %in% c("circle", "square")) {
    warning("`glyph` can only be circle or square so far")
    glyph <- "circle"
  }
  glyph
}
