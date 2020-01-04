mbind <- function(new_mapping = aes(), mapping) {
  
  if (!missing(mapping) && !inherits(mapping, "uneval") &&
      !missing(new_mapping) && !inherits(new_mapping, "uneval")) {
    stop("Mapping should be created with `aes()`.", call. = FALSE)
  }
  
  new_aes(new_mapping %<-% mapping)
}

.get <- function(x, envir = as.environment(-1), mode = "any", ifnotfound,
                 inherits = FALSE) {
  
  if(missing(ifnotfound))
    ifnotfound <- list(NULL)
  
  mget(x = x, envir = envir, mode = mode,
       ifnotfound = ifnotfound,
       inherits = inherits)[[x]]
  
}


get_cdf <- function(M, zeroIgnored = TRUE, ...) {
  
  if(missing(M)) stop("No matrix found; get_cdf requires a valid matrix M is passed.")
  args <- list(...)
  
  cdf <- if(zeroIgnored) {
    
    which_is_not_zero <- if(!is.null(args$which_is_not_zero)) args$which_is_not_zero else M != 0
    stats::ecdf(M[which_is_not_zero])
  } else {
    stats::ecdf(M)
  }
  
  return(cdf)
}

color_warning <- function(envir, args) {
  
  color_key <- .get("color_key", envir = envir)
  color_map <- .get("color_map", envir = envir)
  
  if(is.null(color_key) && is.null(color_map)) {
    
    if(!is.null(args$color_key))
      warning("`color_key` is deprecated now. Please use `color` instead.", call. = FALSE)
       
    if(!is.null(args$color_map))
      warning("`color_map` is deprecated now. Please use `color` instead.", call. = FALSE)   
    
  } else NULL # warning has already been generated
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

rename_mapping <- function(mapping) {
  
  if (!missing(mapping) && !inherits(mapping, "uneval")) {
    stop("Mapping should be created with `aes()`.", call. = FALSE)
  }
  
  names_mapping <- names(mapping)
  if(length(names_mapping) == 0 || !"colour" %in% names_mapping) return(mapping)
  names_mapping[names_mapping == "colour"] <- "color"
  names(mapping) <- names_mapping
  return(mapping)
}

# Pass R CMD check --as-cran
# Suggestion from https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
# The reason to set globalVariables instead of define x, y, ... is because
# the cost of extraction values from large data is very heavy
if(getRversion() >= "3.1.0")  utils::globalVariables(c("..mapping_names", "size", "x", "y", "on", "color"))
