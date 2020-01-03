#' @title rasterly_build
#' @description Produce a rasterly object and return the raster information required to produce an image
#' @param rastObj A rasterly object. It should be a list of environments composed of a \code{rasterly()} and
#' several \code{rasterly_...} layers.
#'
#' @note A rasterly object will never be produced until \code{rasterly_build()} is called.
#'
#' @seealso \link{rasterly}, \link{rasterly_points}, \link{[.rasterly}, \link{[<-.rasterly}
#'
#' @examples
#' r <- data.frame(x = rnorm(1e5), y = rnorm(1e5)) %>%
#'        rasterly(mapping = aes(x = x, y = y)) %>%
#'        rasterly_points(color = fire_map)
#' str(r)
#' p <- rasterly_build(r)
#' str(p)
#' @export
rasterly_build <- function(rastObj) {
  if(missing(rastObj)) stop("No 'rasterly' object provided.", call. = FALSE)
  UseMethod("rasterly_build", rastObj)
}

#' @export
rasterly_build.default <- function(rastObj) {
  stop("'rastObj' is an unkown object.", call. = FALSE)
}

#' @export
rasterly_build.rasterlyBuild <- function(rastObj) {
  warning("'rastObj' is a 'rasterlyBuild' object.", call. = FALSE)
  rastObj
}

#' @export
rasterly_build.rasterly <- function(rastObj) {
  
  rasterly_env <- rastObj[["rasterly_env"]]
  guides_env <- rastObj[["rasterly_guides"]]
  rastObj[["rasterly_env"]] <- NULL
  rastObj[["rasterly_guides"]] <- NULL
  layer_env <- rastObj
  remove(rastObj)
  
  plot_width <- .get("plot_width", envir = rasterly_env)
  plot_height <- .get("plot_height", envir = rasterly_env)
  x_range <- .get("x_range", envir = rasterly_env)
  y_range <- .get("y_range", envir = rasterly_env)
  show_raster <- .get("show_raster", envir = rasterly_env)
  
  # no layers are pipped
  if(is.null(layer_env)) {
    l <- structure(
      list(
        agg = matrix(),
        image = NULL,
        lims = list(
          xlims = list(x_range),
          ylims = list(y_range)
        ),
        x_range = x_range,
        y_range = y_range,
        plot_height = plot_height,
        plot_width = plot_width,
        variable_names = NULL,
        background = get("background", envir = rasterly_env, inherits = FALSE),
        colors = list(get("color", envir = rasterly_env, inherits = FALSE))
      ),
      class = c("rasterlyBuild", "rasterly")
    )
    return(l)
  }
  
  # modify range
  xlims <- list()
  ylims <- list()
  lapply(layer_env,
         function(envir) {
           xlim <- get("xlim", envir = envir, inherits = FALSE)
           ylim <- get("ylim", envir = envir, inherits = FALSE)
           # update ranges
           x_range <<- range(x_range, xlim)
           y_range <<- range(y_range, ylim)
           
           xlims <<- c(xlims, list(xlim))
           ylims <<- c(ylims, list(ylim))
         })
  lims <- list(
    xlims = xlims,
    ylims = ylims
  )
  variable_names <- lapply(layer_env,
                           function(envir) {
                             get("variable_names", envir = envir, inherits = FALSE)
                           })
  # used for lapply scopping
  # if show_raster is TRUE
  image <- NULL
  bg <- c()
  colors <- list()
  # aggregate layers
  agg <- lapply(layer_env,
                function(envir) {
                  # "aggregation" is a list of matrices modified by some specific reduction function
                  # if the length of "aggregation" is one, which means it is a single image
                  # else it should be multiple images
                  xlim <- get("xlim", envir = envir, inherits = FALSE)
                  ylim <- get("ylim", envir = envir, inherits = FALSE)
                  aesthetics <- get("aesthetics",
                                    envir = envir,
                                    inherits = FALSE)
                  if(is.null(aesthetics)) aesthetics <- get("aesthetics",
                                                            envir = rasterly_env,
                                                            inherits = FALSE)
                  color <- get("color",
                               envir = envir,
                               inherits = FALSE)
                  
                  agg <- get_aggregation(
                    plot_width = plot_width,
                    plot_height = plot_height,
                    aesthetics = aesthetics,
                    x_range = x_range, y_range = y_range,
                    xlim = xlim, ylim = ylim,
                    func = get("reduction_func", envir = envir, inherits = FALSE),
                    glyph = get("glyph", envir = envir, inherits = FALSE),
                    group_by_data_table = get("group_by_data_table", envir = envir, inherits = FALSE),
                    cores = get("cores", envir = envir, inherits = FALSE)
                  )

                  len_agg <- length(agg)
                  if(len_agg == 0) stop("No aggregation matrices are found", call. = FALSE)
                  else if(len_agg == 1) {
                    # agg is a matrix
                    agg <- agg[[1]]
                    # default color_map
                    color <- color %||% c('lightblue','darkblue')
                  } else {
                    # default color_key
                    color <- color %||% gg_color_hue(len_agg)
                    stopifnot(
                      length(color) >= len_agg
                    )
                  }
                  # show raster or not
                  if(show_raster) {
                    background <- get("background", envir = envir, inherits = FALSE)
                    bg <<- c(bg, background)
                    colors <<- c(colors, list(color))
                    image <<- as_raster(x = agg,
                                        color = color,
                                        span = get("span", envir = envir, inherits = FALSE),
                                        zeroIgnored = TRUE,
                                        image = image,
                                        background = background,
                                        layout = get("layout", envir = envir, inherits = FALSE))
                  }
                  
                  if(inherits(agg, "matrix")) agg <- list(agg)
                  agg
                }
  )
  # set guides background
  if(!is.null(image)) {
    image <- grDevices::as.raster(image)
    if(!is.null(guides_env)) {
      image <- set_guides(image, 
                          x_range = xlims,
                          y_range = ylims,
                          x_pretty = get("x_pretty", envir = guides_env), 
                          y_pretty = get("y_pretty", envir = guides_env), 
                          panel_background = get("panel_background", envir = guides_env),
                          panel_line = get("panel_line", envir = guides_env),
                          background = bg)
    }
  }
  
  l <- structure(
    list(
      agg = agg,
      image = image,
      lims = lims,
      x_range = x_range,
      y_range = y_range,
      plot_height = plot_height,
      plot_width = plot_width,
      variable_names = variable_names,
      background = bg,
      colors = colors
    ),
    class = c("rasterlyBuild", "rasterly")
  )
  return(l)
}
