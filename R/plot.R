#' @title Generic Plot
#' @description Create a static plot based on \code{rasterly} object. 
#' This function allows users to add axes, legends and other descriptive details when generating `rasterly` objects.
#' @name static
#' @param rasterlyObj A \code{rasterly} object.
#' @param xlim Numeric; the x limits (x1, x2) of the plot. Default is \code{NULL}.
#' @param ylim Numeric; the y limits (y1, y2) of the plot. Default is \code{NULL}.
#' @param xlab Character; the label to be used for the \code{x} axis. Default is \code{NULL}.
#' @param ylab Character; the label to be used for the \code{y} axis. Default is \code{NULL}.
#' @param main Character; the title to be used for the plot. Default is \code{NULL}.
#' @param sub sub Character; a subtitle for the plot. Default is \code{NULL}.
#' @param interpolate Logical. Linearly interpolates the image if \code{TRUE}. Default is \code{FALSE}.
#' @param axes Logical; should axes be drawn? Default is \code{TRUE}, set to \code{FALSE} to hide axes.
#' @param legend Logical. Show a figure legend? Default is \code{TRUE}; set to \code{FALSE} to hide the legend.
#' @param legend_label Character. The label to apply to the figure legend. Default is \code{NULL}, which omits the figure legend label.
#' @param legend_layer  Numeric. Specify the layer level within the \code{rasterly} object. The default layer level is `1`, which represents the uppermost layer.
#' @param legend_main Character. The main title to use within the figure legend. The default is \code{NULL}, which omits the figure legend title.
#' @param axes_gpar Object of class \code{gpar}. This graphical parameter (\code{\link{gpar}}) controls axis color, size, and other aesthetics. 
#' @param label_gpar Object of class \code{gpar}. This graphical parameter (\code{\link{gpar}}) controls label color, size, and other aesthetics. 
#' @param main_gpar Object of class \code{gpar}. This graphical parameter (\code{\link{gpar}}) controls the main title's color, size, and other aesthetics.
#' @param legend_gpar Object of class \code{gpar}. This graphical parameter (\code{\link{gpar}}) controls the legend's color, size, and other aesthetics. 
#' @param name Character. An identifier used to locate the \code{\link{grob}} within the display list and/or as a child of another grob.
#' @param gp A \code{\link{gpar}} object, typically the output from a call to the function \code{grid::gpar}. This argument represents a list of graphical parameter settings.
#' @param vp Object of class \code{\link{viewport}}. If provided, \code{\link{rasterlyGrob}} will pass this argument through to \code{grob}. Default is \code{NULL}.
#' @details We provide three functions to produce static graphics, which is based on the API of \code{grid}, \code{plot} and \code{print}.
#' \itemize{
#'  \item{\code{grid}: The \code{rasterlyGrob} and \code{grid.rasterly} are the most flexible data structure. 
#'  These functions produce a **\code{grob}** object. Users can modify the existing display by the functions provided by \code{grid}}.
#'  \item{\code{plot.rasterly}: The usage of this S3 method is very similar to the classic \code{\link{plot}} function.
#'   Users can set axis limits via \code{xlim}  and \code{ylim}, as well as the corresponding labels using \code{xlab} and \code{ylab}, among other attributes.}
#'  \item{\code{print.rasterly}: This S3 method returns only a basic image raster.}
#' }
#' @import grid
#' @export
rasterlyGrob <- function(rasterlyObj, 
                         xlim = NULL, ylim = NULL, xlab = NULL, ylab = NULL, 
                         main = NULL, sub = NULL, 
                         interpolate = FALSE, axes = TRUE, 
                         legend = TRUE, legend_label = NULL, legend_layer = 1, legend_main = NULL,
                         axes_gpar = grid::gpar(col = "black", cex = 1),
                         label_gpar = grid::gpar(col = "black", cex = 1),
                         main_gpar = grid::gpar(col = "black", cex = 1.5),
                         legend_gpar = grid::gpar(col = "black", cex = 1.5),
                         name = NULL, gp = NULL, vp = NULL) {
  
  if(!is.rasterlyBuild(rasterlyObj)) rasterlyObj <- rasterly_build(rasterlyObj)
  
  image <- rasterlyObj$image
  
  if(is.null(image)) {
    message("No image was found.\n 
             Did you forget to pipe layers or set `show_image = FALSE`?")
    return(grid::grob(name = name, gp = gp, vp = vp))
  }
  
  xrange <- rasterlyObj$x_range
  yrange <- rasterlyObj$y_range
  
  # cut the image vertically
  if(!is.null(xlim)) {
    
    stopifnot(
      exprs = {
        length(xlim) == 2
        is.numeric(xlim)
      }
    )
    xlim <- sort(xlim)
    
    if(xlim[1] < xrange[1]) xlim[1] <- xrange[1]
    if(xlim[2] > xrange[2]) xlim[2] <- xrange[2]
    
    x_start <- floor((xlim[1] - xrange[1])/diff(xrange) * rasterlyObj$plot_width)
    x_end <- ceiling((xlim[2] - xrange[1])/diff(xrange) * rasterlyObj$plot_width)
    image <- image[, x_start:x_end]
  } else xlim <- xrange
  
  
  # cut the image horizontally
  if(!is.null(ylim)) {
    
    stopifnot(
      exprs = {
        length(ylim) == 2
        is.numeric(ylim)
      }
    )
    ylim <- sort(ylim)
    
    if(ylim[1] < yrange[1]) ylim[1] <- yrange[1]
    if(ylim[2] > yrange[2]) ylim[2] <- yrange[2]
    y_end <- rasterlyObj$plot_height - floor((ylim[1] - yrange[1])/diff(yrange) * rasterlyObj$plot_height)
    y_start <- rasterlyObj$plot_height - ceiling((ylim[2] - yrange[1])/diff(yrange) * rasterlyObj$plot_height)
    image <- image[y_start:y_end, ]
  } else ylim <- yrange
  
  xpretty <- grid::grid.pretty(xlim)
  ypretty <- grid::grid.pretty(ylim)
  
  plotGrob <- grid::gTree(
    children = grid::gList(
      grid::rasterGrob(image, 
                       interpolate = interpolate, 
                       height=grid::unit(0.8, "npc"), 
                       width = grid::unit(0.8, "npc"),
                       name = "raster"),
      if(axes) {
        grid::gTree(
          children = grid::gList(
            grid::xaxisGrob(at = (xpretty - min(xlim[1]))/diff(xlim),
                            label = xpretty, 
                            vp = grid::viewport(y=0.6, width = grid::unit(0.8, "npc")),
                            name = "xaxis", 
                            gp = axes_gpar
            ),
            grid::yaxisGrob(at = (ypretty - min(ylim[1]))/diff(ylim),
                            label = ypretty, 
                            vp = grid::viewport(x=0.6, height = grid::unit(0.8, "npc")),
                            name = "yaxis",
                            gp = axes_gpar
            )
          ),
          name = "axes"
        )
      },
      if(!is.null(xlab)) {
        grid::textGrob(
          name = "x label",
          label = as.character(xlab),
          y = grid::unit(0, "npc") - grid::unit(1, "lines"),
          gp = label_gpar
        )
      },
      if(!is.null(ylab)) {
        grid::textGrob(
          name = "y label",
          label = as.character(ylab),
          x = grid::unit(0, "npc") - grid::unit(2, "lines"),
          gp = label_gpar,
          rot = 90
        )
      },
      if(!is.null(sub)) {
        grid::textGrob(
          name = "subtitle",
          label = as.character(sub),
          x = grid::unit(0.2, "npc"),
          y = grid::unit(1, "npc") + grid::unit(.2, "lines"),
          gp = label_gpar
        )
      },
      if(!is.null(main)) {
        grid::textGrob(
          name = "main",
          label = as.character(main),
          y = grid::unit(1, "npc") + grid::unit(1, "lines"),
          vjust = .5,
          gp = main_gpar
        )
      }
    ),
    vp = if(is.null(vp)) 
      grid::dataViewport(xscale = xlim, yscale = ylim, 
                         name = "dataViewport")
    else vp,
    name = "plotGrob"
  )
  
  if(legend) {
    
    if(is.null(legend_label)) {
      
      legendrange <- range(rasterlyObj$agg[[legend_layer]])
      legend_label <- grid::grid.pretty(legendrange)
      legendAt <- legend_label
    } else {
      
      if(is.numeric(legend_label)) {
        legendrange <- range(legend_label)
        legendAt <- grid::grid.pretty(legendrange)
        legend_label <- legendAt
      } else {
        legendrange <- c(1, length(legend_label))
        legendAt <- seq(length(legend_label))
      }
        
    }
    
    legendGrob <- grid::gTree(
      children = grid::gList(
        if(!is.null(legend_main)) {
          grid::textGrob(
            name = "legend_main",
            label = as.character(legend_main),
            x = grid::unit(1, "npc"),
            y = grid::unit(0.85, "npc") + grid::unit(.2, "lines"),
            gp = label_gpar
          )
        },
        grid::yaxisGrob(at = (legendAt - legendrange[1])/diff(range(legendrange)),
                        label = legend_label,
                        vp = grid::viewport(x=0.5, 
                                            height = grid::unit(0.6-1e-2, "npc")),
                        main = FALSE,
                        name = "legendAxis",
                        gp = legend_gpar
        ),
        grid::rasterGrob(matrix(rev(rasterlyObj$colors[[legend_layer]]), ncol = 1), 
                         x = grid::unit(1, "npc"),
                         height = grid::unit(0.6, "npc"), 
                         width = grid::unit(0.03, "npc"),
                         name = "legendRaster",
                         interpolate = (length(rasterlyObj$agg) == 1))
      ),
      name = "legendGrob"
    )
    
    margins <-  c(5.1, 4.1, 3.1, 5.1)
    
  } else {
    
    legendGrob <- grid::grob(name = "legendGrob")
    
    margins <-  c(5.1, 4.1, 3.1, 2.1)
  }
  
  grid::gTree(
    children = grid::gList(
      plotGrob,
      legendGrob
    ),
    vp = grid::plotViewport(margins = margins, 
                            name = "plotViewport"),
    name = name,
    gp = gp
  )
}

#' @export
#' @rdname static
grid.rasterly <- function(rasterlyObj, interpolate = FALSE, axes = TRUE, 
                          xlim = NULL, ylim = NULL, xlab = NULL, ylab = NULL, 
                          main = NULL, sub = NULL, 
                          legend = TRUE, legend_label = NULL, legend_layer = 1, legend_main = NULL,
                          axes_gpar = grid::gpar(col = "black", cex = 1),
                          label_gpar = grid::gpar(col = "black", cex = 1),
                          main_gpar = grid::gpar(col = "black", cex = 1.5),
                          legend_gpar = grid::gpar(col = "black", cex = 1.5),
                          name = NULL, gp = NULL, vp = NULL, ...) {
  rg <- rasterlyGrob(rasterlyObj = rasterlyObj, interpolate = interpolate, axes = axes, 
                     xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, 
                     main = main, sub = sub, 
                     legend = legend, legend_label = legend_label, legend_layer = legend_layer,
                     legend_main = legend_main,
                     axes_gpar = axes_gpar,
                     label_gpar = label_gpar,
                     main_gpar = main_gpar,
                     legend_gpar = legend_gpar,
                     name = name, gp = gp, vp = vp, ...)
  grid::grid.draw(rg)
}

#' @export
#' @rdname static
#' @param x A \code{rasterly} object
#' @param y NULL, will be ignored.
#' @param new.page display on a new page or not.
#' @param ... Other arguments to modify the display.
plot.rasterly <- function(x, y = NULL, xlim = NULL, ylim = NULL, xlab = NULL, ylab = NULL, 
                          main = NULL, legend_main = NULL, sub = NULL, interpolate=FALSE, axes = TRUE, 
                          legend = TRUE, legend_label = NULL, legend_layer = 1, new.page = TRUE, ...) {
  
  if(new.page)
    grid::grid.newpage()
  
  args <- list(...)
  # some possible imput
  axiscolor <- args$axiscolor %||% args$axisColor %||% 
    args$axiscolour %||% args$axisColour %||% 
    args$axiscol %||%  args$axisCol %||% "black"
  axissize <- args$axissize %||% args$axisSize %||% 1
  labelcolor <- args$labelcolor %||% args$labelColor %||% 
    args$labelcolour %||% args$labelColour %||% 
    args$labelcol %||%  args$labelCol %||% "black"
  labelsize <- args$labelsize %||% args$labelSize %||% 1
  maincolor <- args$maincolor %||% args$mainColor %||% 
    args$maincolour %||% args$mainColour %||% 
    args$maincol %||%  args$mainCol %||% "black"
  mainsize <- args$mainsize %||% args$mainSize %||% 1
  legendcolor <- args$legendcolor %||% args$legendColor %||% 
    args$legendcolour %||% args$legendColour %||% 
    args$legendcol %||%  args$legendCol %||% "black"
  legendsize <- args$legendsize %||% args$legendSize %||% 1
  
  grid.rasterly(x, interpolate = interpolate, axes = axes, 
                xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, 
                main = main, sub = sub, 
                legend = legend, legend_label = legend_label, legend_layer = legend_layer, legend_main = legend_main,
                axes_gpar = grid::gpar(col = axiscolor, cex = axissize),
                label_gpar = grid::gpar(col = labelcolor, cex = labelsize),
                main_gpar = grid::gpar(col = maincolor, cex = mainsize),
                legend_gpar = grid::gpar(col = legendcolor, cex = legendsize),
                name = args$name, gp = args$gp, vp = args$vp,  ...)
  return(invisible())
}

#' @export
#' @rdname static
print.rasterly <- function(x, ...) {
  
  if(!is.rasterlyBuild(x)) x <- rasterly_build(x)
  
  grid::grid.newpage()
  
  if(is.null(x$image)) {
    message("No image was found.\n 
            Maybe you forget to pipe layers or set `show_image = FALSE`?")
    invisible(grid::grid.draw(grid::grob()))
  } else {
    invisible(grid::grid.raster(x$image))
  }
}
