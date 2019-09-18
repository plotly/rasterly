#' @export
is.rasterizer <- function(x) {
  inherits(x, "rasterizer")
}

#' @export
is.rasterizer_build <- function(x) {
  inherits(x, "rasterizer_build")
}

#' @export
is.rasterizeLayer <- function(x) {
  inherits(x, "rasterizeLayer")
}

.get <- function(x, envir = parent.frame(), inherits = FALSE) {
  
  m <- mget(x, envir = envir, ifnotfound = list(NULL),
            inherits = inherits)
  return(m[[x]])
}

get_cdf <- function(M, zeroIgnored = TRUE, ...) {
  
  if(missing(M)) stop("No matrix found")
  args <- list(...)
  
  cdf <- if(zeroIgnored) {
    
    which_is_not_zero <- if(!is.null(args$which_is_not_zero)) args$which_is_not_zero else M != 0
    stats::ecdf(M[which_is_not_zero])
  } else {
    stats::ecdf(M)
  }
  
  return(cdf)
}

get_mapped_colour <- function(colour_map = c('lightblue','darkblue'),
                              span = 50) {
  
  # get colour rgb value
  rgb_num <- get_rgb_num(colour_map)
  span <- max(span, length(colour_map))
  # use interpolation to extend colour_map
  col_index <- interpolation(red = rgb_num$red, green = rgb_num$green, blue = rgb_num$blue,
                             span = span)
  
  col_index
}

get_varnames <- function(var_names, dir) {
  varnames <- switch(dir,
                  "x" = var_names[grepl("x", names(var_names))][1],
                  "y" = var_names[grepl("y", names(var_names))][1]
  )
  return(varnames)
}


remove_missing_matrix <- function(m, value = 0) {
  m[!rowSums(!is.finite(m)),]
  # replace all non-finite values with 0
  m[!is.finite(m)] <- value
  m
}