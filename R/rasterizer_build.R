#' @title rasterizer_build
#' @description Execute a "rasterizer" object and return the info to build image
#' @param rastObj A rasterizer object. It should be a list of environments composed of a `rasterizer()` and 
#' several `rasterize_...` layers
#' 
#' @note A rasterizer object will never be implemented until we call `rasterizer_build()`
#' 
#' @seealso \link{rasterizer}, \link{rasterize_points}, \link{[.rasterizer}, \link{[<-.rasterizer}
#' 
#' @examples 
#' r <- data.frame(x = rnorm(1e5), y = rnorm(1e5)) %>% 
#'        rasterizer(mapping = aes(x = x, y = y)) %>%
#'        rasterize_points(colour_map = fire)
#' str(r)
#' p <- rasterizer_build(r)
#' str(p)
#' @export
#' 
rasterizer_build <- function(rastObj) {
  
  if(missing(rastObj) || !is.rasterizer(rastObj)) stop("No 'rasterizer' object", call. = FALSE)
  if(!is.rasterizeLayer(rastObj)) stop("No 'aggregation' layer", call. = FALSE)
  
  rasterizer_env <- rastObj[["rasterizer_env"]]
  rastObj[["rasterizer_env"]] <- NULL
  layer_env <- rastObj
  remove(rastObj)

  plot_width <- .get("plot_width", envir = rasterizer_env)
  plot_height <- .get("plot_height", envir = rasterizer_env)
  x_range <- .get("x_range", envir = rasterizer_env)
  y_range <- .get("y_range", envir = rasterizer_env)
  show_raster <- .get("show_raster", envir = rasterizer_env)
  
  # modify range
  lims <- lapply(layer_env, 
                 function(envir) {
                   xlim <- get("xlim", envir = envir, inherits = FALSE)
                   ylim <- get("ylim", envir = envir, inherits = FALSE)
                   # update ranges
                   x_range <<- range(x_range, xlim)
                   y_range <<- range(y_range, ylim)
                   
                   list(
                     xlim = xlim, 
                     ylim = ylim
                   )
                 })
  variable_names <- lapply(layer_env, 
                           function(envir) {
                             get("variable_names", envir = envir, inherits = FALSE)
                           })
  # used for lapply scopping
  # if show_raster is TRUE
  image <- NULL
  bg <- c()
  colours <- list()
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
                                                            envir = rasterizer_env, 
                                                            inherits = FALSE)
                  
                  start_time <- Sys.time()
                  agg <- get_aggregation(
                    plot_width = plot_width, 
                    plot_height = plot_height,
                    aesthetics = aesthetics,
                    x_range = x_range, y_range = y_range, 
                    xlim = xlim, ylim = ylim,
                    func = get("reduction_func", envir = envir, inherits = FALSE),
                    glyph = get("glyph", envir = envir, inherits = FALSE),
                    group_by_data_table = get("group_by_data_table", envir = envir, inherits = FALSE)
                  )
                  end_time <- Sys.time()
                  print(paste("get_aggregation time:", end_time - start_time))
                  
                  len_agg <- length(agg)
                  if(len_agg == 0) stop("No aggregation matrices are found", call. = FALSE)
                  else if(len_agg == 1) {
                    # agg is a matrix
                    agg <- agg[[1]]
                    class(agg) <- c("rasterizeMatrix", "matrix")
                    colour <- get("colour_map", 
                                  envir = envir, 
                                  inherits = FALSE)
                  } else {
                    # agg is a list
                    agg <- lapply(agg, 
                                  function(a) {
                                    class(a) <- c("rasterizeMatrix", "matrix")
                                    a
                                  })
                    class(agg) <- c("rasterizeList", "list")
                    colour <- get_colour_key(
                      colour_key = get("colour_key", 
                                       envir = envir, 
                                       inherits = FALSE), 
                      n = len_agg,
                      rasterizer_colour_key = get("colour_key", 
                                                  envir = rasterizer_env, 
                                                  inherits = FALSE)
                    )
                  }
                  # show raster or not
                  if(show_raster) {
                    start_time <- Sys.time()
                    background <- get("background", envir = envir, inherits = FALSE)
                    bg <<- c(bg, background)
                    colours <<- c(colours, list(colour))
                    image <<- as.raster(x = agg,    
                                        colour = colour,          
                                        span = get("span", envir = envir, inherits = FALSE),
                                        zeroIgnored = TRUE, 
                                        image = image, 
                                        background = background,
                                        alpha = get("alpha", envir = envir, inherits = FALSE),
                                        layout = get("layout", envir = envir, inherits = FALSE))
                    end_time <- Sys.time()
                    print(paste("get raster time:", end_time - start_time))
                  }
                  
                  if(inherits(agg, "rasterizeMatrix")) agg <- list(agg)
                  agg
                }
  )
  
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
      colours = colours
    ),
    class = c("rasterize", "rasterizer")
  )
  return(l)
}