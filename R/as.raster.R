#' @export
as.raster.rasterizerMatrix <- function(x, colour = c('lightblue','darkblue'), span = 50,
                                       zeroIgnored = TRUE, image = NULL, background = "#FFFFFF",
                                       alpha = 255, layout = c("weighted", "cover")) {
  
  layout <- match.arg(layout)
  span <- max(span, length(colour))
  # get dimension
  dimM <- dim(x)
  which_is_not_zero <- x != 0
  
  if(length(which_is_not_zero) > 0) {
    
    cdf <- get_cdf(M = x, zeroIgnored = TRUE, which_is_not_zero = which_is_not_zero)
    id <- floor(cdf(x) * (span - 1))[which_is_not_zero] + 1
    
    col_index <- get_mapped_color(colour_map = colour,
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
        colours <- get_rgb_num(image[which_is_not_zero])
        red <- (red + colours$red)/2
        green <- (green + colours$green)/2
        blue <- (blue + colours$blue)/2
      } else if (layout == "cover") {
        NULL
      } else stop("Unknown `layout` way; `layout` can only be either `weighted` or `cover`")
    }
    
    image[which_is_not_zero] <- grDevices::rgb(red = red,
                                               green = green,
                                               blue = blue,
                                               alpha = alpha,
                                               maxColorValue = 255)
  } else {
    # x is a zero matrix
    image <- rep(background, dimM[1] * dimM[2])
  }
  
  image <- matrix(image, nrow = dimM[1])
  image[dimM[1]:1, ]
}

#' @export
as.raster.rasterizerList <- function(x, colour = NULL, span = 50,
                                     zeroIgnored = TRUE, image = NULL, background = "#FFFFFF",
                                     alpha = 255, layout = c("weighted", "cover")) {
  
  n <- length(x)
  if(missing(colour) || is.null(colour)) colour <- gg_color_hue(n)
  stopifnot(
    exprs = {
      length(colour) >= n
    }
  )
  
  layout <- match.arg(layout)
  dimM <- dim(x[[1]])
  
  if(layout == "weighted") {
    
    colour_key_rgb_num <- get_rgb_num(colour)
    
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
                       red <<- colour_key_rgb_num$red[i] * M
                       green <<- colour_key_rgb_num$green[i] * M
                       blue <<- colour_key_rgb_num$blue[i] * M
                     } else {
                       red <<- red + colour_key_rgb_num$red[i] * M
                       green <<- green + colour_key_rgb_num$green[i] * M
                       blue <<- blue + colour_key_rgb_num$blue[i] * M
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
    
    colours <- grDevices::rgb(red = red,
                              green = green,
                              blue = blue,
                              alpha = alpha,
                              maxColorValue = 255)
    
    colours[grepl("#000000", colours)] <- background
    
    image <- matrix(colours, nrow = dimM[1])
    image[dimM[1]:1, ]
  } else if (layout == "cover") {
    
    if(is.null(image)) {
      image <- matrix(rep(background, dimM[1]*dimM[2]), nrow = dimM[1])
    }
    
    lapply(1:n,
           function(i){
             image <<- as.raster(x = x[[i]], 
                                 colour = c(background, colour[i]),
                                 span = span,
                                 zeroIgnored = zeroIgnored,
                                 image = image,
                                 background = background,
                                 alpha = alpha, layout = layout)
           })
    image
  } else stop("Unknown `layout` way; `layout` can only be either `weighted` or `cover`")
}