as_raster <- function(x, ...) {
  UseMethod("as_raster", x)
}

as_raster.matrix <- function(x, color = c('lightblue','darkblue'), span = 50,
                             zeroIgnored = TRUE, image = NULL, background = "#FFFFFF",
                             alpha = 1, layout = c("weighted", "cover")) {
  
  layout <- match.arg(layout)
  span <- max(span, length(color))
  # get dimension
  dimM <- dim(x)
  which_is_not_zero <- x != 0
  
  if(length(which_is_not_zero) > 0) {
    
    cdf <- get_cdf(M = x, zeroIgnored = TRUE, which_is_not_zero = which_is_not_zero)
    id <- floor(cdf(x) * (span - 1))[which_is_not_zero] + 1
    
    col_index <- get_mapped_color(color = color,
                                  span = span)
    
    red <- col_index$red[id]
    green <- col_index$green[id]
    blue <- col_index$blue[id]
    
    if(is.null(image)) {
      # build image
      image <- rep(background, dimM[1]*dimM[2])
    } else {
      image <- image[dimM[1]:1, ]
      if(layout == "weighted") {
        colors <- get_rgb_num(image[which_is_not_zero])
        red <- (red + colors$red)/2
        green <- (green + colors$green)/2
        blue <- (blue + colors$blue)/2
      } else if (layout == "cover") {
        NULL
      } else stop("Unknown `layout` method provided; `weighted` or `cover` are currently supported approaches.")
    }
    
    image[which_is_not_zero] <- grDevices::rgb(red = red/255 + 1e-8,
                                               green = green/255 + 1e-8,
                                               blue = blue/255 + 1e-8,
                                               alpha = alpha + 1e-8,
                                               maxColorValue = 1 + 2e-8)
  } else {
    # x is a zero matrix
    image <- rep(background, dimM[1] * dimM[2])
  }
  
  image <- matrix(image, nrow = dimM[1])
  image <- image[dimM[1]:1, ]
  return(image)
}

as_raster.list <- function(x, color = NULL, span = 50,
                           zeroIgnored = TRUE, image = NULL, background = "#FFFFFF",
                           alpha = 1, layout = c("weighted", "cover")) {
  
  n <- length(x)
  if(missing(color) || is.null(color)) color <- gg_color_hue(n)
  stopifnot(
    exprs = {
      length(color) >= n
    }
  )
  
  layout <- match.arg(layout)
  dimM <- dim(x[[1]])
  
  if(layout == "weighted") {
    
    color_key_rgb_num <- get_rgb_num(color)
    # weighted x
    summed_M <- Reduce('+', x)
    # used for divide
    summed_M[summed_M == 0] <- 1
    red <- green <- blue <- matrix()
    hide <- lapply(1:n,
                   function(i){
                     M <- x[[i]]
                     M <- M/summed_M
                     
                     if(i == 1) {
                       red <<- color_key_rgb_num$red[i] * M
                       green <<- color_key_rgb_num$green[i] * M
                       blue <<- color_key_rgb_num$blue[i] * M
                     } else {
                       red <<- red + color_key_rgb_num$red[i] * M
                       green <<- green + color_key_rgb_num$green[i] * M
                       blue <<- blue + color_key_rgb_num$blue[i] * M
                     }
                   })
    
    if(is.null(image)) {
      # build image
      image <- rep(background, dimM[1]*dimM[2])
    } else {
      image <- image[dimM[1]:1, ]
      not_background <- image != background
      image_rgb_num <- get_rgb_num(image[not_background])
      # In R, matrix + vector, vector will be converted to matrix first.
      red[not_background] <- image_rgb_num$red
      green[not_background] <- image_rgb_num$green
      blue[not_background] <- image_rgb_num$blue
    }
    #
    colors <- grDevices::rgb(red = red/255 + 1e-8,
                             green = green/255 + 1e-8,
                             blue = blue/255 + 1e-8,
                             alpha = alpha + 1e-8,
                             maxColorValue = 1 + 2e-8)
    
    colors[grepl("#000000", colors)] <- background
    
    image <- matrix(colors, nrow = dimM[1])
    image <- image[dimM[1]:1, ]
  } else if (layout == "cover") {
    
    if(is.null(image)) {
      image <- matrix(rep(background, dimM[1]*dimM[2]), nrow = dimM[1])
    }
    
    lapply(1:n,
           function(i){
             image <<- as_raster(x = x[[i]],
                                 color = c(background, color[i]),
                                 span = span,
                                 zeroIgnored = zeroIgnored,
                                 image = image,
                                 background = background,
                                 alpha = alpha, layout = layout)
           })
  } else 
    stop("Unknown `layout` method provided; `weighted` or `cover` are currently supported approaches.")
  
  return(image)
}
