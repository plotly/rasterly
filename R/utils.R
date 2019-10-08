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

get_mapped_color <- function(color_map = c('lightblue','darkblue'),
                              span = 50) {
  
  # get color rgb value
  rgb_num <- get_rgb_num(color_map)
  span <- max(span, length(color_map))
  # use interpolation to extend color_map
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

# jump R CMD check
# Suggestion from https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
# The reason to set globleVar instead of define x, y, ... is because
# the cost of extraction values from large data is very heavy
if(getRversion() >= "3.1.0")  utils::globalVariables(c("..mapping_names", "size", "x", "y", "on", "color"))
