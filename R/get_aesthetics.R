get_aesthetics <- function(data, mapping) {

  mapping_names <- names(mapping)

  if(!"x" %in% mapping_names)
    stop("x is missing", call. = FALSE)

  if(!"y" %in% mapping_names)
    stop("y is missing", call. = FALSE)

  if("column" %in% mapping_names) {
    warning("`column` is deprecated, please use `on` instead")
    mapping_names[which(mapping_names == "column")] <- "on"
  }

  if(!all(mapping_names %in% c("x", "y", "on", "colour", "size"))) {
    message("Only `x`, `y`, `on` `size` and `colour` can be passed in so far")
    message("More features are coming!")
  }

  v <- lapply(1:length(mapping),
              function(i) {
                var <- mapping[[i]]
                name <- sub("~", "", rlang::expr_text(var))

                # fastern code
                value <- data[[name]]
                if(is.null(value)) value <- rlang::eval_tidy(rlang::quo(!!var),  data)
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
                #                        "colour" = {
                #                          if(is.character(value)) {
                #                            value <- as.factor(value)
                #                          }
                #                        })
                list(
                  name = name,
                  value = value
                )
              })

  stats::setNames(v, mapping_names)
}
