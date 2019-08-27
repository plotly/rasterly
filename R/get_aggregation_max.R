get_aggregation.max <- function(plot_width, plot_height, aesthetics,
                                x_range, y_range, xlim, ylim, 
                                func, glyph, group_by_data_table, ...) {
  
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
