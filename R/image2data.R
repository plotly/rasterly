#' @title Image ratser to data frame.
#' @description Transform a image raster to a data frame.
#' @param x It could be a rasterly object or a raster image.
#' @param background The background of image raster.
#' @param x_range The range represents image width.
#' @param y_range The range represents image height.
#' @return a \code{data.table} object
#' @seealso \link{ggRasterly}
#' @examples 
#' x <- rnorm(1000, mean = 10)
#' y <- rnorm(1000, mean = 20)
#' color <- sample(1:5, 1000, replace = TRUE)
#' rastObj <- data.frame(x = x, y = y, color = color) %>%
#'        rasterly(mapping = aes(x = x, y = y, color = color)) %>%
#'        rasterly_points()
#' p <- rasterly_build(rastObj)
#' dt <- image2data(p)
#' if(requireNamespace("ggplot2")) {
#'   # Note that each point represents a single pixel in the image
#'   ggplot2::ggplot(dt, mapping = aes(x = x, y = y)) + 
#'     ggplot2::geom_point(color = dt$color, size = 0.5)
#' }
#' @export
image2data <- function(x,
                       background = "white",
                       x_range = NULL,
                       y_range = NULL) {
  if(missing(x)) stop("Missing x")
  UseMethod("image2data", x)
}

#' @export
image2data.rasterly <- function(x, 
                                background = "white",
                                x_range = NULL,
                                y_range = NULL) {
  
  if(!is.rasterlyBuild(x)) x <- rasterly_build(x)
  
  imageData(image = as.matrix(x$image), 
            background = background %||% x$background,
            x_range = x_range %||% x$x_range,
            y_range = y_range %||% x$y_range)
}

#' @export
image2data.raster <- function(x, 
                              background = "white",
                              x_range = NULL,
                              y_range = NULL) {
  imageData(image = as.matrix(x), 
            background = background,
            x_range = x_range,
            y_range = y_range)
}

#' @export
image2data.matrix <- function(x, 
                              background = "white",
                              x_range = NULL,
                              y_range = NULL) {
  
  imageData(image = x, 
            background = background,
            x_range = x_range,
            y_range = y_range)
}

imageData <- function(image, 
                      background = "white",
                      x_range = NULL,
                      y_range = NULL) {
  dimM <- dim(image)
  height <- dimM[1]
  width <- dimM[2]
  
  x_range <- x_range %||% c(1, width)
  y_range <- y_range %||% c(1, height)
  
  y <- rep(seq(from = y_range[2], to = y_range[1], length.out = height), width)
  x <- rep(seq(from = x_range[1], to = x_range[2], length.out = width), each = height)
  
  not_blank_id <- c(!image %in% background)
  y <- y[not_blank_id]
  x <- x[not_blank_id]
  image <- image[not_blank_id]
  data.table::data.table(
    x = x,
    y = y,
    color = image
  )
}