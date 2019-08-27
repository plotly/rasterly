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
