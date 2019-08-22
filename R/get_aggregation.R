get_aggregation <- function(plot_width, plot_height, aesthetics,
                            x_range, y_range, xlim, ylim, func, glyph, group_by_data_table, ...) {
  UseMethod("get_aggregation", func)
}

get_aggregation.sum <- function(plot_width, plot_height, aesthetics,
                                x_range, y_range, xlim, ylim, 
                                func,  glyph, group_by_data_table, ...) {
  
  is_on <- !is.null(aesthetics$on)
  is_size <- !is.null(aesthetics$size)
  is_colour <- !is.null(aesthetics$colour)
  
  L <- if(group_by_data_table) {
    
    display <- aesthetics[,
                          list(
                            display = list(
                              aggregation_sumCpp(plot_width = plot_width, plot_height = plot_height,
                                                 x_range = x_range, y_range = y_range,
                                                 xlim = xlim, ylim = ylim,
                                                 x = x,
                                                 y = y,
                                                 on = if(is_on) on else numeric(0),
                                                 size = if(is_size) size else numeric(0),
                                                 glyph = glyph)
                            )
                          ),
                          by = if(is_colour) colour else NULL]
    remove(aesthetics)
    display$display
  } else {
    
    if(is_colour) {
      levels <- unique(aesthetics$colour)
      # agg_sumCpp return a list
      agg_sumCpp(L = lapply(1:length(levels), function(i) matrix(0, nrow = plot_height, ncol = plot_width)),
                 levels = levels,
                 category = aesthetics$colour,
                 plot_width = plot_width, plot_height = plot_height,
                 x_range = x_range, y_range = y_range,
                 xlim = xlim, ylim = ylim,
                 x = aesthetics$x,
                 y = aesthetics$y,
                 on = if(is_on) aesthetics$on else numeric(0),
                 size = if(is_size) aesthetics$size else numeric(0),
                 glyph = glyph)
    } else {
      list(
        aggregation_sumCpp(plot_width = plot_width, plot_height = plot_height,
                           x_range = x_range, y_range = y_range,
                           xlim = xlim, ylim = ylim,
                           x = aesthetics$x,
                           y = aesthetics$y,
                           on = if(is_on) aesthetics$on else numeric(0),
                           size = if(is_size) aesthetics$size else numeric(0),
                           glyph = glyph)
      )
    }
  }
  return(L)
}

get_aggregation.default <- get_aggregation.sum

get_aggregation.any <- function(plot_width, plot_height, aesthetics,
                                x_range, y_range, xlim, ylim, 
                                func, glyph, group_by_data_table, ...) {
  
  is_on <- !is.null(aesthetics$on)
  is_size <- !is.null(aesthetics$size)
  is_colour <- !is.null(aesthetics$colour)
  
  L <- if(group_by_data_table) {
    
    display <- aesthetics[,
                          list(
                            display = list(
                              aggregation_anyCpp(plot_width = plot_width, plot_height = plot_height,
                                                 x_range = x_range, y_range = y_range,
                                                 xlim = xlim, ylim = ylim,
                                                 x = x,
                                                 y = y,
                                                 on = if(is_on) on else numeric(0),
                                                 size = if(is_size) size else numeric(0),
                                                 glyph = glyph)
                            )
                          ),
                          by = if(is_colour) colour else NULL]
    remove(aesthetics)
    display$display
  } else {
    if(is_colour) {
      levels <- unique(aesthetics$colour)
      # agg_sumCpp return a list
      agg_anyCpp(L = lapply(1:length(levels), function(i) matrix(0, nrow = plot_height, ncol = plot_width)),
                 levels = levels,
                 category = aesthetics$colour,
                 plot_width = plot_width, plot_height = plot_height,
                 x_range = x_range, y_range = y_range,
                 xlim = xlim, ylim = ylim,
                 x = aesthetics$x,
                 y = aesthetics$y,
                 on = if(is_on) aesthetics$on else numeric(0),
                 size = if(is_size) aesthetics$size else numeric(0),
                 glyph = glyph)
    } else {
      list(
        aggregation_anyCpp(plot_width = plot_width, plot_height = plot_height,
                           x_range = x_range, y_range = y_range,
                           xlim = xlim, ylim = ylim,
                           x = aesthetics$x,
                           y = aesthetics$y,
                           on = if(is_on) aesthetics$on else numeric(0),
                           size = if(is_size) aesthetics$size else numeric(0),
                           glyph = glyph)
      )
    }
  }
  return(L)
}

get_aggregation.first <- function(plot_width, plot_height, aesthetics,
                                  x_range, y_range, xlim, ylim, 
                                  func, glyph, group_by_data_table, ...) {
  
  
  if(is.null(aesthetics$on)) stop("No `on` argument. Which variable is the first value encountered?")
  is_size <- !is.null(aesthetics$size)
  is_colour <- !is.null(aesthetics$colour)
  
  L <- if(group_by_data_table) {
    
    display <- aesthetics[,
                          list(
                            display = list(
                              aggregation__firstCpp(plot_width = plot_width, plot_height = plot_height,
                                                    x_range = x_range, y_range = y_range,
                                                    xlim = xlim, ylim = ylim,
                                                    x = x,
                                                    y = y,
                                                    on = on,
                                                    size = if(is_size) size else numeric(0),
                                                    glyph = glyph)
                            )
                          ),
                          by = if(is_colour) colour else NULL]
    remove(aesthetics)
    display$display
  } else {
    
    if(is_colour) {
      levels <- unique(aesthetics$colour)
      # agg_sumCpp return a list
      agg_firstCpp(L = lapply(1:length(levels), function(i) matrix(0, nrow = plot_height, ncol = plot_width)),
                   levels = levels,
                   category = aesthetics$colour,
                   plot_width = plot_width, plot_height = plot_height,
                   x_range = x_range, y_range = y_range,
                   xlim = xlim, ylim = ylim,
                   x = aesthetics$x,
                   y = aesthetics$y,
                   on = aesthetics$on,
                   size = if(is_size) aesthetics$size else numeric(0),
                   glyph = glyph)
    } else {
      list(
        aggregation_firstCpp(plot_width = plot_width, plot_height = plot_height,
                             x_range = x_range, y_range = y_range,
                             xlim = xlim, ylim = ylim,
                             x = aesthetics$x,
                             y = aesthetics$y,
                             on = aesthetics$on,
                             size = if(is_size) aesthetics$size else numeric(0),
                             glyph = glyph)
      )
    }
  }
  return(L)
}

get_aggregation.last <- function(plot_width, plot_height, aesthetics,
                                 x_range, y_range, xlim, ylim, 
                                 func, glyph, group_by_data_table, ...) {
  
  if(is.null(aesthetics$on)) stop("No `on` argument. Which variable is the last value encountered?")
  is_size <- !is.null(aesthetics$size)
  is_colour <- !is.null(aesthetics$colour)
  
  L <- if(group_by_data_table) {
    
    display <- aesthetics[,
                          list(
                            display = list(
                              aggregation__lastCpp(plot_width = plot_width, plot_height = plot_height,
                                                   x_range = x_range, y_range = y_range,
                                                   xlim = xlim, ylim = ylim,
                                                   x = x,
                                                   y = y,
                                                   on = on,
                                                   size = if(is_size) size else numeric(0),
                                                   glyph = glyph)
                            )
                          ),
                          by = if(is_colour) colour else NULL]
    remove(aesthetics)
    display$display
  } else {
    
    if(is_colour) {
      levels <- unique(aesthetics$colour)
      # agg_sumCpp return a list
      agg_lastCpp(L = lapply(1:length(levels), function(i) matrix(0, nrow = plot_height, ncol = plot_width)),
                  levels = levels,
                  category = aesthetics$colour,
                  plot_width = plot_width, plot_height = plot_height,
                  x_range = x_range, y_range = y_range,
                  xlim = xlim, ylim = ylim,
                  x = aesthetics$x,
                  y = aesthetics$y,
                  on = aesthetics$on,
                  size = if(is_size) aesthetics$size else numeric(0),
                  glyph = glyph)
    } else {
      list(
        aggregation_lastCpp(plot_width = plot_width, plot_height = plot_height,
                            x_range = x_range, y_range = y_range,
                            xlim = xlim, ylim = ylim,
                            x = aesthetics$x,
                            y = aesthetics$y,
                            on = aesthetics$on,
                            size = if(is_size) aesthetics$size else numeric(0),
                            glyph = glyph)
      )
    }
  }
  return(L)
}

get_aggregation.m2 <- function(plot_width, plot_height, aesthetics,
                               x_range, y_range, xlim, ylim, 
                               func, glyph, group_by_data_table, ...) { 
  
  if(is.null(aesthetics$on)) stop("No `on` argument. Which variable is used for sum of square differences from the mean?")
  is_size <- !is.null(aesthetics$size)
  is_colour <- !is.null(aesthetics$colour)
  
  aesthetics$on <- (aesthetics$on - mean(aesthetics$on))^2
  get_aggregation.sum(plot_width = plot_width,
                      plot_height = plot_height, 
                      aesthetics = aesthetics,
                      x_range = x_range, 
                      y_range = y_range, 
                      xlim = xlim, 
                      ylim = ylim, 
                      func = NULL, 
                      glyph = glyph, 
                      group_by_data_table = group_by_data_table, ...)
}

get_aggregation.max <- function(plot_width, plot_height, aesthetics,
                                x_range, y_range, xlim, ylim, 
                                func, glyph, ...) {
  
  if(is.null(aesthetics$on)) stop("No `on` argument. Which variable is used to maximize?")
  is_size <- !is.null(aesthetics$size)
  is_colour <- !is.null(aesthetics$colour)
  
  L <- if(group_by_data_table) {
    
    display <- aesthetics[,
                          list(
                            display = list(
                              aggregation__maxCpp(plot_width = plot_width, plot_height = plot_height,
                                                  x_range = x_range, y_range = y_range,
                                                  xlim = xlim, ylim = ylim,
                                                  x = x,
                                                  y = y,
                                                  on = on,
                                                  size = if(is_size) size else numeric(0),
                                                  glyph = glyph)
                            )
                          ),
                          by = if(is_colour) colour else NULL]
    remove(aesthetics)
    display$display
  } else {
    
    if(is_colour) {
      levels <- unique(aesthetics$colour)
      # agg_sumCpp return a list
      agg_maxCpp(L = lapply(1:length(levels), function(i) matrix(0, nrow = plot_height, ncol = plot_width)),
                 levels = levels,
                 category = aesthetics$colour,
                 plot_width = plot_width, plot_height = plot_height,
                 x_range = x_range, y_range = y_range,
                 xlim = xlim, ylim = ylim,
                 x = aesthetics$x,
                 y = aesthetics$y,
                 on = aesthetics$on,
                 size = if(is_size) aesthetics$size else numeric(0),
                 glyph = glyph)
    } else {
      list(
        aggregation_maxCpp(plot_width = plot_width, plot_height = plot_height,
                           x_range = x_range, y_range = y_range,
                           xlim = xlim, ylim = ylim,
                           x = aesthetics$x,
                           y = aesthetics$y,
                           on = aesthetics$on,
                           size = if(is_size) aesthetics$size else numeric(0),
                           glyph = glyph)
      )
    }
  }
  return(L)
}

get_aggregation.mean <- function(plot_width, plot_height, aesthetics,
                                 x_range, y_range, xlim, ylim, 
                                 func, glyph, group_by_data_table, ...) {
  
  is_on <- !is.null(aesthetics$on)
  is_size <- !is.null(aesthetics$size)
  is_colour <- !is.null(aesthetics$colour)
  
  if(!is_on) {
    # default setting
    aesthetics[, on := aesthetics$y]
  }
  
  L <- if(group_by_data_table) {
    
    display <- aesthetics[,
                          list(
                            display = list(
                              {
                                if(identical(aesthetics$on, aesthetics$y) || identical(aesthetics$on, aesthetics$x)) {
                                  aggregation_meanCpp(plot_width = plot_width, plot_height = plot_height,
                                                      x_range = x_range, y_range = y_range,
                                                      xlim = xlim, ylim = ylim,
                                                      x = x,
                                                      y = y,
                                                      on = on,
                                                      size = if(is_size) size else numeric(0),
                                                      glyph = glyph)
                                  
                                } else {
                                  # it would be twice slower
                                  sum_matrix <- aggregation_sumCpp(plot_width = plot_width, plot_height = plot_height,
                                                                   x_range = x_range, y_range = y_range,
                                                                   xlim = xlim, ylim = ylim,
                                                                   x = x,
                                                                   y = y,
                                                                   on = on,
                                                                   size = if(is_size) size else numeric(0),
                                                                   glyph = glyph)
                                  
                                  count_matrix <- aggregation_sumCpp(plot_width = plot_width, plot_height = plot_height,
                                                                     x_range = x_range, y_range = y_range,
                                                                     xlim = xlim, ylim = ylim,
                                                                     x = x,
                                                                     y = y,
                                                                     on = numeric(0),
                                                                     size = if(is_size) size else numeric(0),
                                                                     glyph = glyph)
                                  count_matrix[count_matrix == 0] <- 1
                                  sum_matrix/count_matrix
                                }
                              }
                            )
                          ),
                          by = if(is_colour) colour else NULL]
    display$display
  } else {
    
    if(is_colour) {
      
      levels <- unique(aesthetics$colour)
      
      if(identical(aesthetics$on, aesthetics$y) || identical(aesthetics$on, aesthetics$x)) {
        agg_meanCpp(L = lapply(1:length(levels), function(i) matrix(0, nrow = plot_height, ncol = plot_width)),
                    levels = levels,
                    category = aesthetics$colour,
                    plot_width = plot_width, plot_height = plot_height,
                    x_range = x_range, y_range = y_range,
                    xlim = xlim, ylim = ylim,
                    x = aesthetics$x,
                    y = aesthetics$y,
                    on = aesthetics$on,
                    size = if(is_size) aesthetics$size else numeric(0),
                    glyph = glyph)
      } else {
        sum_matrix_list <- agg_sumCpp(L = lapply(1:length(levels), function(i) matrix(0, nrow = plot_height, ncol = plot_width)),
                                      levels = levels,
                                      category = aesthetics$colour,
                                      plot_width = plot_width, plot_height = plot_height,
                                      x_range = x_range, y_range = y_range,
                                      xlim = xlim, ylim = ylim,
                                      x = aesthetics$x,
                                      y = aesthetics$y,
                                      on = aesthetics$on,
                                      size = if(is_size) aesthetics$size else numeric(0),
                                      glyph = glyph)
        count_matrix_list <- agg_sumCpp(L = lapply(1:length(levels), function(i) matrix(0, nrow = plot_height, ncol = plot_width)),
                                        levels = levels,
                                        category = aesthetics$colour,
                                        plot_width = plot_width, plot_height = plot_height,
                                        x_range = x_range, y_range = y_range,
                                        xlim = xlim, ylim = ylim,
                                        x = aesthetics$x,
                                        y = aesthetics$y,
                                        on = numeric(0),
                                        size = if(is_size) aesthetics$size else numeric(0),
                                        glyph = glyph)
        lapply(1:length(levels), 
               function(i) {
                 sum_matrix <- sum_matrix_list[[i]]
                 count_matrix <- count_matrix_list[[i]]
                 count_matrix[count_matrix == 0] <- 1
                 sum_matrix/count_matrix
               }
        )
      }
    } else {
      
      if(identical(aesthetics$on, aesthetics$y) || identical(aesthetics$on, aesthetics$x)) {
        list(
          aggregation_meanCpp(plot_width = plot_width, plot_height = plot_height,
                              x_range = x_range, y_range = y_range,
                              xlim = xlim, ylim = ylim,
                              x = aesthetics$x,
                              y = aesthetics$y,
                              on = aesthetics$on,
                              size = if(is_size) aesthetics$size else numeric(0),
                              glyph = glyph)
        )
      } else {
        
        # it would be twice slower
        sum_matrix <- aggregation_sumCpp(plot_width = plot_width, plot_height = plot_height,
                                         x_range = x_range, y_range = y_range,
                                         xlim = xlim, ylim = ylim,
                                         x = aesthetics$x,
                                         y = aesthetics$y,
                                         on = aesthetics$on,
                                         size = if(is_size) aesthetics$size else numeric(0),
                                         glyph = glyph)
        
        count_matrix <- aggregation_sumCpp(plot_width = plot_width, plot_height = plot_height,
                                           x_range = x_range, y_range = y_range,
                                           xlim = xlim, ylim = ylim,
                                           x = aesthetics$x,
                                           y = aesthetics$y,
                                           on = numeric(0),
                                           size = if(is_size) aesthetics$size else numeric(0),
                                           glyph = glyph)
        count_matrix[count_matrix == 0] <- 1
        list(sum_matrix/count_matrix)
      }
    }
  }
  return(L)
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