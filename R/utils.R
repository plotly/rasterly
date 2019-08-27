#' @export
is.canvas <- function(x) {
  inherits(x, "canvas")
}

#' @export
is.rasterizer <- function(x) {
  inherits(x, "rasterizer")
}

#' @export
is.aggregation <- function(x) {
  inherits(x, "aggregation")
}

#' @export
aes <- ggplot2::aes

#' @export
`%>%` <- magrittr::`%>%`

.get <- function(x, envir = parent.frame(), inherits = FALSE) {
  
  m <- mget(x, envir = envir, ifnotfound = list(NULL),
            inherits = inherits)
  return(m[[x]])
}

`%||%` <- function(x, y) {
  if (is.null(x)) {
    y
  }
  else {
    x
  }
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

get_mapped_color <- function(colour_map = c('lightblue','darkblue'),
                             span = 50) {
  
  # get color rgb value
  rgb_num <- get_rgb_num(colour_map)
  span <- max(span, length(colour_map))
  # use interpolation to extend colour_map
  col_index <- interpolation(red = rgb_num$red, green = rgb_num$green, blue = rgb_num$blue,
                             span = span)
  
  col_index
}


remove_missing_matrix <- function(m, value = 0) {
  m[!rowSums(!is.finite(m)),]
  # replace all non-finite values with 0
  m[!is.finite(m)] <- value
  m
}

reduction_func_args <- function(func, aesthetics, ...) {
  
  args <- list(...)
  args$colour <- aesthetics$colour$value
  
  if(func != "") {
    tryCatch(
      {
        reduction_func <- get(func)
        func_args <- methods::formalArgs(func)
        args[Filter(function(name) name %in% func_args, names(args))]
        
      },
      error = function(e) {
        message(paste("unkwon function", func))
        message("reduction function is replaced to `sum`")
      }
    )
  }
  args
}

zeroOneTransform <- function(z) {
  
  minz <- min(z)
  maxz <- max(z)
  M <- matrix((z - minz)/(maxz - minz), nrow = dim(z)[1])
  
  return(M)
}