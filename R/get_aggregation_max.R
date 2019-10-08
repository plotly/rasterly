get_aggregation.max <- function(plot_width, plot_height, aesthetics,
                                x_range, y_range, xlim, ylim, 
                                func, glyph, group_by_data_table, ...) {
  
  if(is.null(aesthetics$on)) stop("No `on` argument was provided. Which variable should be used when calculating the maximum value for aggregation?")
  is_size <- !is.null(aesthetics$size)
  is_color <- !is.null(aesthetics$color)
  
  L <- if(group_by_data_table) {
    
    display <- aesthetics[,
                          list(
                            display = list(
                              aggregation_maxCpp(plot_width = plot_width, plot_height = plot_height,
                                                 x_range = x_range, y_range = y_range,
                                                 xlim = xlim, ylim = ylim,
                                                 x = x,
                                                 y = y,
                                                 on = on,
                                                 size = if(is_size) size else numeric(0),
                                                 glyph = glyph)
                            )
                          ),
                          by = if(is_color) color else NULL]
    remove(aesthetics)
    display$display
  } else {
    
    if(is_color) {
      levels <- unique(aesthetics$color)
      # agg_sumCpp return a list
      agg_maxCpp(L = lapply(1:length(levels), function(i) matrix(0, nrow = plot_height, ncol = plot_width)),
                 levels = levels,
                 category = aesthetics$color,
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
