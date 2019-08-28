#' @export
#' @import grid

grid.rasterizer <- function (widget, ..., name = NULL, gp = grid::gpar(), draw = TRUE, vp = NULL) {
  
  lg <- rasterizerGrob(widget, ..., name = name, gp = gp, vp = vp)
  
  if (draw)
    grid::grid.draw(lg)
  
  invisible(lg)
}

#' @export
plot.rasterizer <- function (x, y = NULL, ...) {
  if (!is.null(y)) warning("argument y is ignored")
  g <- grid.rasterizer(x, ...)
  invisible(g)
}

#' @export
rasterizerGrob <- function(rastObj, ..., interpolate = FALSE,
                           name = NULL, gp = grid::gpar(), vp = NULL) {
  
  if(missing(rastObj) || !is.rasterize(rastObj)) 
    stop("No 'rasterize' object. Forget to pass to `execute()`?", call. = FALSE)
  if(is.rasterizeLayer(rastObj)) 
    stop("'RasterizeLayer' is passed. Forget to pass to `execute()`?", call. = FALSE)
  if(is.null(rastObj$image)) 
    stop("No image is found. Set `show_raster = TRUE`", call. = FALSE)
  
  args <- list(...)
  margins <- args$margins %||% c(3.6, 4.1, 2.1, 1.1)
  bounding_box <- args$bounding_box %||% "white"
  # title default settings
  title <- args$title
  title_fontsize <- args$title_fontsize %||% 20
  title_fontfamily <- args$title_fontfamily %||% "serif"
  title_fontface <- args$title_fontface %||% "bold"
  title_location <- args$title_location %||% 0.8
  title_hvjust <- args$title_hvjust %||% c(0.5, 0.5)
  title_col <- args$title_col %||% "black"
  # label default settings
  var_names <- unlist(rastObj$variable_names)
  xlabel <- args$xlabel %||% var_names["x"]
  ylabel <- args$ylabel %||% var_names["y"]
  label_fontsize <- args$label_fontsize %||% 12
  label_fontfamily <- args$label_fontfamily %||% "serif"
  label_fontface <- args$label_fontface %||% "plain"
  label_location <- args$label_location %||% c(-2.5, -3.5)
  label_hvjust <- args$label_hvjust %||% c(0.5, 0.5)
  label_col <- args$label_col %||% "black"
  # axis 
  axis_col <- args$axis_col %||% "black"
  axis_fontsize <- args$axis_fontsize %||% 8

  rGrob <- grid::gTree(
    children = grid::gList(
      grid::rectGrob(gp = grid::gpar(col = NA,
                                     fill = bounding_box),
                     name = "bounding box"),
      grid::gTree(
        children = grid::gList(
          # Axes
          grid::gTree(
            children = grid::gList(
              grid::xaxisGrob(
                at = grid::grid.pretty(rastObj$x_range),
                name = "x axis",
                gp = grid::gpar(
                  col = axis_col,
                  fontsize = axis_fontsize
                )
              ),
              grid::yaxisGrob(
                at = grid::grid.pretty(rastObj$y_range),
                name = "y axis",
                gp = grid::gpar(
                  col = axis_col,
                  fontsize = axis_fontsize
                )
              )
            ), 
            name = "axes"
          ),
          # Label
          grid::gTree(
            children = grid::gList(
              grid::textGrob(
                label = xlabel,
                name = "x label",
                y = grid::unit(label_location[1], "lines"),
                vjust = label_hvjust[2],
                hjust = label_hvjust[1],
                gp = grid::gpar(
                  fontsize = label_fontsize,
                  fontfamily = label_fontfamily,
                  fontface = label_fontface,
                  col = label_col
                )
              ),
              grid::textGrob(
                label = ylabel,
                name = "y label",
                x = grid::unit(label_location[2], "lines"),
                vjust = label_hvjust[2],
                hjust = label_hvjust[1],
                rot = 90,
                gp = grid::gpar(
                  fontsize = label_fontsize,
                  fontfamily = label_fontfamily,
                  fontface = label_fontface,
                  col = label_col
                )
              ),
              if(!is.null(title)) {
                grid::textGrob(
                  label = title,
                  name = "title",
                  y = grid::unit(1, "npc") + grid::unit(title_location, "lines"),
                  vjust = title_hvjust[2],
                  hjust = title_hvjust[1],
                  gp = grid::gpar(
                    fontsize = title_fontsize,
                    fontfamily = title_fontfamily,
                    fontface = title_fontface,
                    col = title_col
                  )
                )
              } else {
                grid::grob(name = "title")
              }
            ), 
            name = "labels"
          ),
          # image
          grid::rasterGrob(
            image = rastObj$image,
            interpolate = interpolate,
            width = grid::unit(1.01, "npc"),
            height = grid::unit(1.01, "npc"),
            name = "image"
          ),
          grid::clipGrob(name = "clipping region")
        ),
        vp = grid::vpStack(
          grid::plotViewport(margins = margins, name = "plotViewport"),
          grid::dataViewport(xscale = grDevices::extendrange(rastObj$x_range), 
                             yscale = grDevices::extendrange(rastObj$y_range),
                             name = "dataViewport")
        ),
        name = "rasterizer plot"
      )
    ),
    name = name, vp = vp, gp = gp
  )
}
