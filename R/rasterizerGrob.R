#' @title Render a "rasterizer" object
#' @description Render a "rasterizer" object 
#' @param rastObj A "rasterizer" object
#' @param ... To specify layout aesthetics. See more in details
#' @param interpolate A logical value indicating whether to linearly interpolate the image 
#' (the alternative is to use nearest-neighbour interpolation, which gives a more blocky result).
#' @param name A character identifier.
#' @param gp An object of class gpar, typically the output from a call to the function gpar. 
#' This is basically a list of graphical parameter settings.
#' @param vp A Grid viewport object (or NULL).
#' @details
#' Set ... in `grid.rasterizer()` and `rasterizerGrob()`
#' \itemize{
#'  \item{}{title: The title of output graph}
#'  \item{}{title_fontsize: title fontsize}
#'  \item{}{title_fontfamily: title fontfamily}
#'  \item{}{title_fontface: title fontface}
#'  \item{}{title_location: title location}
#'  \item{}{title_hvjust: title hvjust, a length 2 vector represents "hjust" and "vjust" correspondingly}
#'  \item{}{title_col: title colour}
#'  \item{}{xlabel: x label, default setting is the "x" name in mapping system}
#'  \item{}{ylabel: y label, default setting is the "y" name in mapping system}
#'  \item{}{label_fontsize: label fontsize}
#'  \item{}{label_fontfamily: label fontfamily}
#'  \item{}{label_fontface: label fontface}
#'  \item{}{label_location: label location}
#'  \item{}{label_hvjust: label hvjust, a length 2 vector represents "hjust" and "vjust" correspondingly}
#'  \item{}{label_col: label colour}
#'  \item{}{axis_col: axis colour}
#'  \item{}{axis_fontsize: axis fontsize}
#'  \item{}{image_width: image width}
#'  \item{}{image_height: image height}
#' }
#' @usage 
#' grid.rasterizer(rastObj, ..., interpolate = FALSE,
#'   name = NULL, gp = grid::gpar(), vp = NULL)
#'     
#' rasterizerGrob(rastObj, ..., interpolate = FALSE, 
#'   name = NULL, gp = grid::gpar(), vp = NULL)
#' @export
#' @import grid
#' @seealso \code{\link{plot.rasterizer}}, \code{\link{plotly.rasterizer}}
#' @examples 
#' iris %>% 
#'   rasterizer(plot_height = 20, 
#'              plot_width = 20, 
#'              mapping = aes(x = Sepal.Width, y = Petal.Length, colour = Species)) %>% 
#'   rasterize_points() %>% 
#'   rasterizer_build() %>% 
#'   grid.rasterizer()

grid.rasterizer <- function (rastObj, ..., interpolate = FALSE, 
                             name = NULL, gp = grid::gpar(), vp = NULL) {
  
  lg <- rasterizerGrob(rastObj, ..., interpolate = interpolate, 
                       name = name, gp = gp, vp = vp)
  
  grid::grid.draw(lg)
}

#' @export
#' @inherit grid.rasterizer
#' @usage 
#' grid.rasterizer(rastObj, ..., interpolate = FALSE,
#'   name = NULL, gp = grid::gpar(), vp = NULL)
#'     
#' rasterizerGrob(rastObj, ..., interpolate = FALSE, 
#'   name = NULL, gp = grid::gpar(), vp = NULL)
rasterizerGrob <- function(rastObj, ..., interpolate = FALSE,
                           name = NULL, gp = grid::gpar(), vp = NULL) {
  
  if(missing(rastObj) || !is.rasterizer(rastObj)) stop("No 'rasterizer' object.", call. = FALSE)
  if(is.rasterizeLayer(rastObj) && !is.rasterizer_build(rastObj)) {
    # to a 'rasterizer_build' object
    rastObj['show_raster', which = 1] <- TRUE
    rastObj <- rasterizer_build(rastObj)
  }
  if(is.null(rastObj$image)) {
    message("No image is found. Set `show_raster = TRUE`", call. = FALSE)
    return(grid::nullGrob(name = "rasterizer plot"))
  }
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
  xlabel <- args$xlabel %||% var_names[grepl("x", names(var_names))][1]
  ylabel <- args$ylabel %||% var_names[grepl("y", names(var_names))][1]

  label_fontsize <- args$label_fontsize %||% 12
  label_fontfamily <- args$label_fontfamily %||% "serif"
  label_fontface <- args$label_fontface %||% "plain"
  label_location <- args$label_location %||% c(-2.5, -3.5)
  label_hvjust <- args$label_hvjust %||% c(0.5, 0.5)
  label_col <- args$label_col %||% "black"
  # axis 
  axis_col <- args$axis_col %||% "black"
  axis_fontsize <- args$axis_fontsize %||% 8
  # image 
  image_width <- args$image_width %||% grid::unit(1.01, "npc")
  image_height <- args$image_height %||% grid::unit(1.01, "npc")

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
            width = image_width,
            height = image_height,
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


#' Plot "rasterizer" object.
#'
#' This is a wrapper for \code{grid.rasterizer()} to simplify the plotting.
#'
#' @param x a "rasterizer" object to be plotted
#' @param y NULL, will be ignored.
#' @param ... parameters passed to \code{grid.rasterizer} and \code{rasterizerGrob}
#'
#' @return invisible()
#'
#'
#' @seealso \code{\link{grid.rasterizer}}, \code{\link{rasterizerGrob}}
#'
#' @examples
#' mtcars %>% 
#'   rasterizer(plot_height = 20, 
#'              plot_width = 20, 
#'              mapping = aes(x = hp, y = mpg, colour = cyl)) %>% 
#'   rasterize_points() %>% 
#'   rasterizer_build() -> p
#'   plot(x = p)
#' @export
plot.rasterizer <- function (x, y = NULL, ...) {
  if (!is.null(y)) warning("argument y is ignored")
  grid::grid.newpage()
  g <- grid.rasterizer(x, ...)
  invisible(g)
}