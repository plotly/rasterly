get_aggregation <- function(plot_width, plot_height, aesthetics,
                            x_range, y_range, func, ...) {
  UseMethod("get_aggregation", func)
}

get_aggregation.sum <- function(plot_width, plot_height, aesthetics,
                                x_range, y_range,
                                func, ...) {


  is_colour <- TRUE
  if(is.null(aesthetics[["colour"]]$value)) is_colour <- FALSE
  is_on <- TRUE
  if(is.null(aesthetics[["on"]]$value)) is_on <- FALSE
  is_size <- TRUE
  if(is.null(aesthetics[["size"]]$value) && is.null(aesthetics[["pixel_share"]])) is_size <- FALSE

  if(is_colour) {

    aesthetics_table <- get_aesthetics_table(aesthetics, as_data_table = TRUE,
                                             is_on = is_on,
                                             is_size = is_size)

    display <- aesthetics_table[,
                                list(
                                  display = list(
                                    aggregation_sumCpp(plot_width = plot_width, plot_height = plot_height,
                                                       x_range = x_range, y_range = y_range,
                                                       x = x,
                                                       y = y,
                                                       on = if(is_on) on else numeric(0),
                                                       size = if(is_size) size else numeric(0),
                                                       glyph = aesthetics$glyph)
                                  )
                                ),
                                by = colour]
    display$display
  } else {

    aesthetics_table <- get_aesthetics_table(aesthetics,
                                             as_data_table = FALSE,
                                             is_on = is_on,
                                             is_size = is_size)

    list(
      aggregation_sumCpp(plot_width = plot_width, plot_height = plot_height,
                         x_range = x_range, y_range = y_range,
                         x = aesthetics_table$x,
                         y = aesthetics_table$y,
                         on = if(is_on) aesthetics_table$on else numeric(0),
                         size = if(is_size) aesthetics_table$size else numeric(0),
                         glyph = aesthetics$glyph)
    )
  }
}

get_aggregation.default <- get_aggregation.sum

get_aggregation.any <- function(plot_width, plot_height, aesthetics,
                                x_range, y_range,
                                func, ...) {

  is_colour <- TRUE
  if(is.null(aesthetics[["colour"]]$value)) is_colour <- FALSE
  is_on <- TRUE
  if(is.null(aesthetics[["on"]]$value)) is_on <- FALSE
  is_size <- TRUE
  if(is.null(aesthetics[["size"]]$value)) is_size <- FALSE

  if(is_colour) {

    aesthetics_table <- get_aesthetics_table(aesthetics, as_data_table = TRUE,
                                             is_on = is_on,
                                             is_size = is_size)

    display <- aesthetics_table[,
                                list(
                                  display = list(
                                    aggregation_anyCpp(plot_width = plot_width, plot_height = plot_height,
                                                       x_range = x_range, y_range = y_range,
                                                       x = x,
                                                       y = y,
                                                       on = if(is_on) on else numeric(0),
                                                       size = if(is_size) size else numeric(0),
                                                       glyph = aesthetics$glyph)
                                  )
                                ),
                                by = colour]
    display$display
  } else {
    aesthetics_table <- get_aesthetics_table(aesthetics,
                                             as_data_table = FALSE,
                                             is_on = is_on,
                                             is_size = is_size)

    list(
      aggregation_anyCpp(plot_width = plot_width, plot_height = plot_height,
                         x_range = x_range, y_range = y_range,
                         x = aesthetics_table$x,
                         y = aesthetics_table$y,
                         on = if(is_on) aesthetics_table$on else numeric(0),
                         size = if(is_size) aesthetics_table$size else numeric(0),
                         glyph = aesthetics$glyph)
    )
  }
}

get_aggregation.mean <- function(plot_width, plot_height, aesthetics,
                                 x_range, y_range,
                                 func, ...) {

  is_colour <- TRUE
  if(is.null(aesthetics[["colour"]]$value)) is_colour <- FALSE
  is_on <- TRUE
  if(is.null(aesthetics[["on"]]$value)) is_on <- FALSE
  is_size <- TRUE
  if(is.null(aesthetics[["size"]]$value)) is_size <- FALSE

  xname <- aesthetics$x$name
  yname <- aesthetics$y$name
  onname <- aesthetics$on$name

  if(is_colour) {

    aesthetics_table <- get_aesthetics_table(aesthetics, as_data_table = TRUE,
                                             is_on = is_on,
                                             is_size = is_size)

    if(is.null(onname)) {
      # default setting
      onname <- yname
      aesthetics_table[, on := aesthetics$y$value]
    }

    display <- aesthetics_table[,
                                list(
                                  display = list(
                                    {
                                      if(onname == xname || onname == yname) {
                                        aggregation_meanCpp(plot_width = plot_width, plot_height = plot_height,
                                                            x_range = x_range, y_range = y_range,
                                                            x = x,
                                                            y = y,
                                                            on = on,
                                                            size = if(is_size) size else numeric(0),
                                                            glyph = aesthetics$glyph)

                                      } else {
                                        # it would be twice slower
                                        sum_matrix <- aggregation_sumCpp(plot_width = plot_width, plot_height = plot_height,
                                                                         x_range = x_range, y_range = y_range,
                                                                         x = x,
                                                                         y = y,
                                                                         on = on,
                                                                         size = if(is_size) size else numeric(0),
                                                                         glyph = aesthetics$glyph)

                                        count_matrix <- aggregation_sumCpp(plot_width = plot_width, plot_height = plot_height,
                                                                           x_range = x_range, y_range = y_range,
                                                                           x = x,
                                                                           y = y,
                                                                           on = numeric(0),
                                                                           size = if(is_size) size else numeric(0),
                                                                           glyph = aesthetics$glyph)
                                        count_matrix[count_matrix == 0] <- 1
                                        sum_matrix/count_matrix
                                      }
                                    }
                                  )
                                ),
                                by = colour]
    display$display
  } else {

    aesthetics_table <- get_aesthetics_table(aesthetics,
                                             as_data_table = FALSE,
                                             is_on = is_on,
                                             is_size = is_size)

    if(is.null(onname)) {
      # default setting
      onname <- yname
      aesthetics_table$on <- aesthetics$y$value
    }

    if(onname == xname || onname == yname) {

      display <- aggregation_meanCpp(plot_width = plot_width, plot_height = plot_height,
                                     x_range = x_range, y_range = y_range,
                                     x = aesthetics_table$x,
                                     y = aesthetics_table$y,
                                     on = aesthetics_table$on,
                                     size = if(is_size) aesthetics_table$size else numeric(0),
                                     glyph = aesthetics$glyph)

    } else {

      # it would be twice slower
      sum_matrix <- aggregation_sumCpp(plot_width = plot_width, plot_height = plot_height,
                                       x_range = x_range, y_range = y_range,
                                       x = aesthetics_table$x,
                                       y = aesthetics_table$y,
                                       on = aesthetics_table$on,
                                       size = if(is_size) aesthetics_table$size else numeric(0),
                                       glyph = aesthetics$glyph)

      count_matrix <- aggregation_sumCpp(plot_width = plot_width, plot_height = plot_height,
                                         x_range = x_range, y_range = y_range,
                                         x = aesthetics_table$x,
                                         y = aesthetics_table$y,
                                         on = numeric(0),
                                         size = if(is_size) aesthetics_table$size else numeric(0),
                                         glyph = aesthetics$glyph)
      count_matrix[count_matrix == 0] <- 1
      display <- sum_matrix/count_matrix
    }
    list(display)
  }
}

get_aggregation.first <- function(plot_width, plot_height, aesthetics,
                                  x_range, y_range,
                                  func, ...) {

  is_colour <- TRUE
  if(is.null(aesthetics[["colour"]]$value)) is_colour <- FALSE
  is_on <- TRUE
  if(is.null(aesthetics[["on"]]$value)) is_on <- FALSE
  if(!is_on) warning("No `on` argument. Which variable is the first value encountered?")
  is_size <- TRUE
  if(is.null(aesthetics[["size"]]$value)) is_size <- FALSE

  if(is_colour) {

    aesthetics_table <- get_aesthetics_table(aesthetics, as_data_table = TRUE,
                                             is_on = is_on,
                                             is_size = is_size)

    display <- aesthetics_table[,
                                list(
                                  display = list(
                                    aggregation_firstCpp(plot_width = plot_width, plot_height = plot_height,
                                                         x_range = x_range, y_range = y_range,
                                                         x = x,
                                                         y = y,
                                                         on = if(is_on) on else numeric(0),
                                                         size = if(is_size) size else numeric(0),
                                                         glyph = aesthetics$glyph)
                                  )
                                ),
                                by = colour]
    display$display
  } else {
    aesthetics_table <- get_aesthetics_table(aesthetics,
                                             as_data_table = FALSE,
                                             is_on = is_on,
                                             is_size = is_size)

    list(
      aggregation_firstCpp(plot_width = plot_width, plot_height = plot_height,
                           x_range = x_range, y_range = y_range,
                           x = aesthetics_table$x,
                           y = aesthetics_table$y,
                           on = if(is_on) aesthetics_table$on else numeric(0),
                           size = if(is_size) aesthetics_table$size else numeric(0),
                           glyph = aesthetics$glyph)
    )
  }
}


get_aggregation.last <- function(plot_width, plot_height, aesthetics,
                                 x_range, y_range,
                                 func, ...) {

  is_colour <- TRUE
  if(is.null(aesthetics[["colour"]]$value)) is_colour <- FALSE
  is_on <- TRUE
  if(is.null(aesthetics[["on"]]$value)) is_on <- FALSE
  if(!is_on) stop("No `on` argument. Which variable is the last value encountered?")
  is_size <- TRUE
  if(is.null(aesthetics[["size"]]$value)) is_size <- FALSE

  if(is_colour) {

    aesthetics_table <- get_aesthetics_table(aesthetics, as_data_table = TRUE,
                                             is_on = is_on,
                                             is_size = is_size)

    display <- aesthetics_table[,
                                list(
                                  display = list(
                                    aggregation_lastCpp(plot_width = plot_width, plot_height = plot_height,
                                                        x_range = x_range, y_range = y_range,
                                                        x = x,
                                                        y = y,
                                                        on = on,
                                                        size = if(is_size) size else numeric(0),
                                                        glyph = aesthetics$glyph)
                                  )
                                ),
                                by = colour]
    display$display
  } else {
    aesthetics_table <- get_aesthetics_table(aesthetics,
                                             as_data_table = FALSE,
                                             is_on = is_on,
                                             is_size = is_size)

    list(
      aggregation_lastCpp(plot_width = plot_width, plot_height = plot_height,
                          x_range = x_range, y_range = y_range,
                          x = aesthetics_table$x,
                          y = aesthetics_table$y,
                          on = aesthetics_table$on,
                          size = if(is_size) aesthetics_table$size else numeric(0),
                          glyph = aesthetics$glyph)
    )
  }
}

get_aggregation.m2 <- function(plot_width, plot_height, aesthetics,
                               x_range, y_range,
                               func, ...) {

  is_colour <- TRUE
  if(is.null(aesthetics[["colour"]]$value)) is_colour <- FALSE
  is_on <- TRUE
  if(is.null(aesthetics[["on"]]$value)) is_on <- FALSE
  if(!is_on) stop("No `on` argument. Which variable is used for sum of square differences from the mean?")
  is_size <- TRUE
  if(is.null(aesthetics[["size"]]$value)) is_size <- FALSE

  if(is_colour) {

    aesthetics_table <- get_aesthetics_table(aesthetics, as_data_table = TRUE,
                                             is_on = is_on,
                                             is_size = is_size)
    aesthetics_table$on <- (aesthetics_table$on - mean(aesthetics_table$on))^2
    display <- aesthetics_table[,
                                list(
                                  display = list(
                                    aggregation_sumCpp(plot_width = plot_width, plot_height = plot_height,
                                                       x_range = x_range, y_range = y_range,
                                                       x = x,
                                                       y = y,
                                                       on = on,
                                                       size = if(is_size) size else numeric(0),
                                                       glyph = aesthetics$glyph)
                                  )
                                ),
                                by = colour]
    display$display
  } else {
    aesthetics_table <- get_aesthetics_table(aesthetics,
                                             as_data_table = FALSE,
                                             is_on = is_on,
                                             is_size = is_size)
    aesthetics_table$on <- (aesthetics_table$on - mean(aesthetics_table$on))^2

    list(
      aggregation_sumCpp(plot_width = plot_width, plot_height = plot_height,
                         x_range = x_range, y_range = y_range,
                         x = aesthetics_table$x,
                         y = aesthetics_table$y,
                         on = aesthetics_table$on,
                         size = if(is_size) aesthetics_table$size else numeric(0),
                         glyph = aesthetics$glyph)
    )
  }
}

get_aggregation.max <- function(plot_width, plot_height, aesthetics,
                                x_range, y_range,
                                func, ...) {

  is_colour <- TRUE
  if(is.null(aesthetics[["colour"]]$value)) is_colour <- FALSE
  is_on <- TRUE
  if(is.null(aesthetics[["on"]]$value)) is_on <- FALSE
  if(!is_on) stop("No `on` argument. Which variable is used to maximize?")
  is_size <- TRUE
  if(is.null(aesthetics[["size"]]$value)) is_size <- FALSE

  if(is_colour) {

    aesthetics_table <- get_aesthetics_table(aesthetics, as_data_table = TRUE,
                                             is_on = is_on,
                                             is_size = is_size)
    display <- aesthetics_table[,
                                list(
                                  display = list(
                                    aggregation_maxCpp(plot_width = plot_width, plot_height = plot_height,
                                                       x_range = x_range, y_range = y_range,
                                                       x = x,
                                                       y = y,
                                                       on = on,
                                                       size = if(is_size) size else numeric(0),
                                                       glyph = aesthetics$glyph)
                                  )
                                ),
                                by = colour]
    display$display
  } else {
    aesthetics_table <- get_aesthetics_table(aesthetics,
                                             as_data_table = FALSE,
                                             is_on = is_on,
                                             is_size = is_size)


    list(
      aggregation_maxCpp(plot_width = plot_width, plot_height = plot_height,
                         x_range = x_range, y_range = y_range,
                         x = aesthetics_table$x,
                         y = aesthetics_table$y,
                         on = aesthetics_table$on,
                         size = if(is_size) aesthetics_table$size else numeric(0),
                         glyph = aesthetics$glyph)
    )
  }
}
################################################ helper function ###################################################
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

get_aesthetics_table <- function(aesthetics, as_data_table = FALSE,
                                 is_on = FALSE,
                                 is_size = FALSE) {

  # data.table is used when original data grouped by colour
  # The main reason to do so is because loading data in list is much faster than data.table
  if(as_data_table) {
    # xy
    aesthetics_table <- data.table::data.table(
      x = aesthetics[["x"]]$value,
      y = aesthetics[["y"]]$value,
      colour = aesthetics[["colour"]]$value
    )

    # on which column
    if(is_on) aesthetics_table[, on := aesthetics[["on"]]$value]
    # the size

    if(is_size) {

      if(is.null(aesthetics[["pixel_share"]])) {
        size <- aesthetics[["size"]]$value
        # standardized size
        aesthetics_table[, size := floor((size - min(size))/(max(size) - min(size)) * (aesthetics$extend_rate - 1))]
      } else {
        size <- aesthetics[["pixel_share"]]
        if(!is.numeric(size)) stop("`Size` must be numerical")
        if(length(size) > 1) {
          warning("Only the first one will be used as size")
          size <- size[1]
        }
        if(size < 1) {
          warning("`Size` should be larger or equal to 1")
          size <- 1
        }
        aesthetics_table[, size := size]
      }
    }
  } else {
    # xy
    aesthetics_table <- list(
      x = aesthetics[["x"]]$value,
      y = aesthetics[["y"]]$value
    )

    # on which column
    if(is_on) aesthetics_table$on <- aesthetics[["on"]]$value
    # the size
    if(is_size) {
      # standardized size
      size <- aesthetics[["size"]]$value
      aesthetics_table$size <- floor((size - min(size))/(max(size) - min(size)) * (aesthetics$extend_rate - 1))
    }
  }

  aesthetics_table
}
