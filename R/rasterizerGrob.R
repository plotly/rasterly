#' @export
#' @import grid
#' 

grid.rasterizer <- function (widget, ..., name = NULL, gp = grid::gpar(), draw = TRUE, vp = NULL) {
  
  lg <- rasterizerGrob(widget, ..., name = name, gp = gp, vp = vp)
  
  if (draw)
    grid::grid.draw(lg)
  
  invisible(lg)
}

#' @export
rasterizerGrob <- function(rastObj, ..., interpolate = FALSE,
                           name = NULL, gp = grid::gpar(), vp = NULL) {
  
  if(missing(rastObj) || !is.rasterizer(rastObj)) stop("No 'rasterizer' object", call. = FALSE)
  if(is.canvas(rastObj)) stop("'Canvas' is passed. `Aggregation_...()` layers and
                              `rasterizer()` is needed", call. = FALSE)
  if(is.aggregation(rastObj)) stop("'Aggregation' layer is passed. Forget to pass to `rasterizer()`?", call. = FALSE)
  if(is.null(rastObj$image)) stop("No image is found. Set `show_raster = TRUE`", call. = FALSE)
  
  args <- list(...)
  margins <- if(is.null(args$margins)) c(3.6, 4.1, 2.1, 1.1) else args$margins
  bounding_box <- ifelse(is.null(args$bounding_box), "white", args$bounding_box)
  # title default settings
  title <- args$title
  title_fontsize <- ifelse(is.null(args$title_fontsize), 20, args$title_fontsize)
  title_fontfamily <- ifelse(is.null(args$title_fontfamily), "serif", args$title_fontfamily)
  title_fontface <- ifelse(is.null(args$title_fontface), "bold", args$title_fontface)
  title_location <- ifelse(is.null(args$title_location), 0.8, args$title_location)
  title_hvjust <- if(is.null(args$title_hvjust)) c(0.5, 0.5) else args$title_hvjust
  title_col <- ifelse(is.null(args$title_col), "black", args$title_col)
  # label default settings
  var_names <- unlist(rastObj$variable_names)
  xlabel <- ifelse(is.null(args$xlabel), var_names["x"], args$xlabel) 
  ylabel <- ifelse(is.null(args$ylabel), var_names["y"], args$ylabel) 
  label_fontsize <- ifelse(is.null(args$label_fontsize), 12, args$label_fontsize)
  label_fontfamily <- ifelse(is.null(args$label_fontfamily), "serif", args$label_fontfamily)
  label_fontface <- ifelse(is.null(args$label_fontface), "plain", args$label_fontface)
  label_location <- if(is.null(args$label_location)) c(-2.5, -3.5) else args$label_location
  label_hvjust <- if(is.null(args$label_hvjust)) c(0.5, 0.5) else args$label_hvjust
  label_col <- ifelse(is.null(args$label_col), "black", args$label_col)
  # axis colour
  axis_col <- ifelse(is.null(args$axis_col), "black", args$axis_col)

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
                gp = grid::gpar(col = axis_col)
              ),
              grid::yaxisGrob(
                at = grid::grid.pretty(rastObj$y_range),
                name = "y axis",
                gp = grid::gpar(col = axis_col)
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
