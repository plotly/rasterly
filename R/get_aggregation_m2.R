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