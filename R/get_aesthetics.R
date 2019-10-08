get_aesthetics <- function(data = NULL, mapping = aes(), variable_check = FALSE, ...) {

  if(is.null(data)) stop("The 'data' aesthetic cannot be NULL; please supply valid data.")
  if(!is.data.frame(data)) stop(paste(deparse(substitute(data)), "is not a data frame."), call. = FALSE)
  if(!data.table::is.data.table(data)) data <- data.table::as.data.table(data)

  if(rlang::is_empty(mapping)) stop("No aesthetics were supplied; please provide aesthetics for the data to rasterize.")

  mapping_names <- names(mapping)

  if(!"x" %in% mapping_names)
    stop("x is missing", call. = FALSE)

  if(!"y" %in% mapping_names)
    stop("y is missing", call. = FALSE)

  if("column" %in% mapping_names) {
    warning("`column` is deprecated, please use `on` instead")
    mapping_names[which(mapping_names == "column")] <- "on"
  }

  if(!all(mapping_names %in% c("x", "y", "on", "color", "size"))) {
    message("Only `x`, `y`, `on` `size` and `color` parameters are currently accepted.")
    message("Additional aesthetics will be supported in the next release.")
  }

  column_names <- colnames(data)
  variable_names <- sapply(mapping, function(var) sub("~", "", rlang::expr_text(var)))

  if(all(variable_names %in% column_names) && all(!duplicated(variable_names))) {

    matchNameId <- function(nameA, nameB) {sapply(nameB, function (x) which(nameA == x))}

    column_names[matchNameId(column_names, variable_names)] <- mapping_names
    colnames(data) <- column_names

    # removing unused variables can reduce memory usage, but can slow execution time
    if(length(variable_names) != length(column_names) && variable_check) data <- data[, ..mapping_names]

  } else {

    for(i in seq_along(mapping)) {

      var <- mapping[[i]]
      name <- sub("~", "", rlang::expr_text(var))

      # fastern code
      value <- data[[name]]
      if(is.null(value)) value <- rlang::eval_tidy(rlang::quo(!!var),  data)

      data[[mapping_names[i]]] <- value
    }

    if(variable_check) data <- data[, ..mapping_names]
  }

  args <- list(...)
  abs_size <- args$size
  max_size <- if(is.null(args$max_size)) 2 else args$max_size

  if(!is.null(data$size) || !is.null(abs_size)) {

    if(is.null(abs_size)) {
      # standardized size
      std <- function(size, max_size) {floor((size - min(size))/(max(size) - min(size)) * (max_size - 1))}
      data[, size := std(data$size, max_size)]
    } else {
      data[, size := abs_size]
    }
  }
  return(data)
}


# TODO: argument check, but it is so expensive!
#                 switch(mapping_names[i],
#                        "x" = {
#                          if(!is.numeric(value))
#                            stop(paste("x variable:", paste0("`",name, "`"), "is not a numerical column"),
#                                 call. = FALSE)
#                          if(is.infinite(value) || is.nan(value))
#                            stop(paste("x variable:", paste0("`",name, "`"), "has `NaN` value or `infinite` value"),
#                                 call. = FALSE)
#                        },
#                        "y" = {
#                          if(!is.numeric(value))
#                            stop(paste("y variable:", paste0("`",name, "`"), 'is not a numerical column'),
#                                 call. = FALSE)
#                          if(is.infinite(value) || is.nan(value))
#                            stop(paste("y variable:", paste0("`",name, "`"), "has `NaN` value or `infinite` value"),
#                                 call. = FALSE)
#                        },
#                        "on" = {
#                            if(!is.numeric(value)) {
#                              warning(paste("on variable:", paste0("`",name, "`"), "is not a numerical column"),
#                                      call. = FALSE)
#                              value <- as.numeric(as.factor(value))
#                            }
#                        },
#                        "color" = {
#                          if(is.character(value)) {
#                            value <- as.factor(value)
#                          }
#                        })
