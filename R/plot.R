#' @title Static rasterly graphics
#' @description Render a \code{rasterly} object. 
#' This object can involve axes, legend, etc to better convert graphics to data.
#' @name static
#' @param raterlyObj A \code{rasterly} object.
#' @param xlim The x limits (x1, x2) of the plot.
#' @param ylim The y limits (x1, x2) of the plot.
#' @param xlab A y label for the plot.
#' @param ylab A x label for the plot.
#' @param main A title for the plot.
#' @param sub A sub title for the plot.
#' @param interpolate A logical value indicating whether to linearly interpolate the image
#' (the alternative is to use nearest-neighbour interpolation, which gives a more blocky result).
#' @param axes Show axes or not.
#' @param legend Show legend or not.
#' @param legendlabel The label of legend.
#' @param legendlayer The legend represents which layer in \code{rasterly} object. Default is 1.
#' @param legendmain The main title of legend
#' @param axes_gpar \code{\link{gpar}} of axes. It is used to modify the axes' color, size and other aesthetics attributes.
#' @param label_gpar \code{\link{gpar}} of label. It is used to modify the labels' color, size and other aesthetics attributes.
#' @param main_gpar \code{\link{gpar}} of main. It is used to modify the main title's color, size and other aesthetics attributes.
#' @param legend_gpar \code{\link{gpar}} of legend It is used to modify the main title's color, size and other aesthetics attributes.
#' @param name a character identifier for the grob. Used to find the grob on the display list and/or as a child of another grob.
#' @param gp A gpar object, typically the output from a call to the function \code{gpar}. This is basically a list of graphical parameter settings.
#' @param vp a \code{\link{viewport}} object (or \code{NULL}).
#' @details We provide three functions to produce static graphics, which is based on the API of `grid`, `plot` and `print`.
#' \itemize{
#'  \item{\code{grid}: The \code{rasterlyGrob} and \code{grid.rasterly} are the most flexible data structure. 
#'  It is nothing but a **\code{grob}**. Users can modify the existing display by the functions provided by \code{grid}}.
#'  \item{\code{plot.rasterly}: It is a S3 method. The usage is very similar to the classic `plot` function. 
#'  Users can set the x limits, y limits, x label, y label and etc.}
#'  \item{\code{print.rasterly}: It is a S3 method, however, it only provides the most basic image raster.}
#' }
#' @import grid
#' @export
rasterlyGrob <- function(rasterlyObj, 
                         xlim = NULL, ylim = NULL, xlab = NULL, ylab = NULL, 
                         main = NULL, sub = NULL, 
                         interpolate = FALSE, axes = TRUE, 
                         legend = TRUE, legendlabel = NULL, legendlayer = 1, legendmain = NULL,
                         axes_gpar = grid::gpar(col = "black", cex = 1),
                         label_gpar = grid::gpar(col = "black", cex = 1),
                         main_gpar = grid::gpar(col = "black", cex = 1.5),
                         legend_gpar = grid::gpar(col = "black", cex = 1.5),
                         name = NULL, gp = NULL, vp = NULL) {
  
  if(!is.rasterlyBuild(rasterlyObj)) rasterlyObj <- rasterly_build(rasterlyObj)
  
  image <- rasterlyObj$image
  
  if(is.null(image)) {
    message("No image was found.\n 
            Maybe you forget to pipe layers or set `show_image = FALSE`?")
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
    
    if(is.null(legendlabel)) {
      
      legendrange <- range(rasterlyObj$agg[[legendlayer]])
      legendlabel <- grid::grid.pretty(legendrange)
      legendAt <- legendlabel
    } else {
      
      if(is.numeric(legendlabel)) {
        legendrange <- range(legendlabel)
        legendAt <- grid::grid.pretty(legendrange)
        legendlabel <- legendAt
      } else {
        legendrange <- c(1, length(legendlabel))
        legendAt <- seq(length(legendlabel))
      }
        
    }
    
    legendGrob <- grid::gTree(
      children = grid::gList(
        if(!is.null(legendmain)) {
          grid::textGrob(
            name = "legendmain",
            label = as.character(legendmain),
            x = grid::unit(1, "npc"),
            y = grid::unit(0.85, "npc") + grid::unit(.2, "lines"),
            gp = label_gpar
          )
        },
        grid::yaxisGrob(at = (legendAt - legendrange[1])/diff(range(legendrange)),
                        label = legendlabel,
                        vp = grid::viewport(x=0.5, 
                                            height = grid::unit(0.6-1e-2, "npc")),
                        main = FALSE,
                        name = "legendAxis",
                        gp = legend_gpar
        ),
        grid::rasterGrob(matrix(rev(rasterlyObj$colors[[legendlayer]]), ncol = 1), 
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
                          legend = TRUE, legendlabel = NULL, legendlayer = 1, legendmain = NULL,
                          axes_gpar = grid::gpar(col = "black", cex = 1),
                          label_gpar = grid::gpar(col = "black", cex = 1),
                          main_gpar = grid::gpar(col = "black", cex = 1.5),
                          legend_gpar = grid::gpar(col = "black", cex = 1.5),
                          name = NULL, gp = NULL, vp = NULL, ...) {
  rg <- rasterlyGrob(rasterlyObj = rasterlyObj, interpolate = interpolate, axes = axes, 
                     xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, 
                     main = main, sub = sub, 
                     legend = legend, legendlabel = legendlabel, legendlayer = legendlayer,
                     legendmain = legendmain,
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
                          main = NULL, legendmain = NULL, sub = NULL, interpolate=FALSE, axes = TRUE, 
                          legend = TRUE, legendlabel = NULL, legendlayer = 1, new.page = TRUE, ...) {
  
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
                legend = legend, legendlabel = legendlabel, legendlayer = legendlayer, legendmain = legendmain,
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
