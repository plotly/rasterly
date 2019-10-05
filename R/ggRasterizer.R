ggRasterly <- function(data = NULL,
                         mapping = aes(),
                         ...,
                         plot_width = 600, plot_height = 600,
                         x_range = NULL, y_range = NULL,
                         background = "white",
                         colour_map = c('lightblue','darkblue'),
                         colour_key = NULL,
                         show_raster = TRUE,
                         drop_data = FALSE,
                         variable_check = FALSE) {
  
  rastObj <- rasterly(data = data,
                        mapping = mapping,
                        ...,
                        plot_width = plot_width, 
                        plot_height = plot_height,
                        x_range = x_range, 
                        y_range = y_range,
                        background = background,
                        colour_map = colour_map,
                        colour_key = colour_key,
                        show_raster = show_raster,
                        drop_data = drop_data,
                        variable_check = variable_check) %>% 
    rasterize_points() %>% 
    rasterly_build()
  
  ggObj <- ggplot2::ggplot(
    data = data.frame(x = seq(rastObj$x_range[1], rastObj$x_range[2], length.out = rastObj$plot_width),
                      y = seq(rastObj$y_range[1], rastObj$y_range[2], length.out = rastObj$plot_height)),
    mapping = aes(x = x, y = y)
  )
  
  # get pretty
  pretty <- gg_pretty(ggObj)
  # guide background
  theme <- ggObj$theme
  
  image <- reset_image_bg(image = rastObj$image, 
                          x_range = pretty[[1]]$x_range,
                          y_range = pretty[[1]]$y_range,
                          x_minor = pretty[[1]]$x_minor, # multiple facets?
                          y_minor = pretty[[1]]$y_minor, # multiple facets?
                          panel_background = theme$panel.background$fill %||% "grey92",
                          panel_line = theme$panel.grid$colour %||% "white",
                          background = rastObj$background)
  
  
  
  ggObj <- ggObj + 
    ggplot2::annotation_custom(grid::rasterGrob(image), 
                               xmin = -Inf, xmax = Inf, 
                               ymin = -Inf, ymax = Inf) + 
    ggplot2::theme(
      axis.ticks = ggplot2::element_line(),
      axis.text.x = ggplot2::element_text(),
      axis.text.y = ggplot2::element_text(),
      axis.title = ggplot2::element_text(),
      axis.title.x = ggplot2::element_text(),
      axis.title.y = ggplot2::element_text()
    )
}

gg_pretty <- function(ggObj) {
  
  build <- ggplot2::ggplot_build(ggObj)
  panel_params <- build$layout$panel_params
  
  lapply(panel_params, 
         function(panel_param) {
           list(
             x_major = panel_param$x$breaks,
             x_minor = panel_param$x$minor_breaks,
             x_range = panel_param$x.range,
             y_major = panel_param$y$breaks,
             y_minor = panel_param$y$minor_breaks,
             y_range = panel_param$y.range
           )
         })
}

reset_image_bg <- function(image = NULL, 
                           x_range = NULL,
                           y_range = NULL,
                           x_minor = NULL, 
                           y_minor = NULL, 
                           panel_background ="grey92",
                           panel_line = "white",
                           background = "white") {
  
  if(is.null(image)) return(grid::rectGrob())
  
  dimI <- dim(image)
  
  x_range <- x_range %||% c(1, dimI[2])
  y_range <- y_range %||% c(1, dimI[1])
  
  stopifnot(
    exprs = {
      length(x_range) == 2
      length(y_range) == 2
    }
  )
  
  x_minor <- x_minor %||% pretty.default(1:dimI[2])
  y_minor <- y_minor %||% pretty.default(1:dimI[1])
  
  x_pos <- round(((x_minor - x_range[1])/diff(x_range)) * (dimI[2] - 1) + 1)
  y_pos <- round(((y_minor - y_range[1])/diff(y_range)) * (dimI[2] - 1) + 1)
  
  image[image == background] <- panel_background
  image <- matrix(image, nrow = dimI[1])
  
  for(x in x_pos) {
    column <- image[, x]
    column[column == panel_background] <- panel_line
    image[, x] <- column
  }
  
  for(y in y_pos) {
    row <- image[y, ]
    row[row == panel_background] <- panel_line
    image[y, ] <- row
  }
  
  return(image)
}

