#' @export
get_raster_2D <- function(M, colour_map = c('lightblue','darkblue'), span = 50,
                          zeroIgnored = TRUE, image = NULL, background = "#FFFFFF",
                          alpha = 255, layout = c("weighted", "cover")) {

  layout <- match.arg(layout)
  span <- max(span, length(colour_map))
  # get dimension
  dimM <- dim(M)
  which_is_not_zero <- M != 0

  if(length(which_is_not_zero) > 0) {

    cdf <- get_cdf(M = M, zeroIgnored = TRUE, which_is_not_zero = which_is_not_zero)
    id <- floor(cdf(M) * (span - 1))[which_is_not_zero] + 1

    col_index <- get_mapped_color(colour_map = colour_map,
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
    # M is a zero matrix
    image <- rep(background, dimM[1] * dimM[2])
  }

  image <- matrix(image, nrow = dimM[1])
  image[dimM[1]:1, ]
}

#' @export
get_raster_3D <- function(L, colour_key = NULL, layout = c("weighted", "cover"), span = 50,
                          zeroIgnored = TRUE, image = NULL, background = "#FFFFFF",
                          alpha = 255) {

  n <- length(L)
  if(missing(colour_key) || is.null(colour_key)) colour_key <- gg_color_hue(n)
  stopifnot(
    exprs = {
      length(colour_key) >= n
    }
  )

  layout <- match.arg(layout)
  dimM <- dim(L[[1]])

  if(layout == "weighted") {

    colour_key_rgb_num <- get_rgb_num(colour_key)

    # weighted L
    summed_M <- Reduce('+', L)
    # used for divide
    summed_M[summed_M == 0] <- 1
    red <- green <- blue <- matrix()
    hide <- lapply(1:n,
                   function(i){
                     M <- L[[i]]
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
             image <<- get_raster_2D(M = L[[i]], colour_map = c(background, colour_key[i]),
                                     span = span/10,
                                     zeroIgnored = zeroIgnored,
                                     image = image,
                                     background = background,
                                     alpha = alpha, layout = "cover")
           })
    image
  } else stop("Unknown `layout` way; `layout` can only be either `weighted` or `cover`")
}

get_cdf <- function(M, zeroIgnored = TRUE, ...) {

  if(missing(M)) stop("No matrix found")
  args <- list(...)

  cdf <- if(zeroIgnored) {

    which_is_not_zero <- if(!is.null(args$which_is_not_zero)) args$which_is_not_zero else M != 0
    stats::ecdf(M[which_is_not_zero])
  } else {
    stats::ecdf(M)
  }

  return(cdf)
}

get_mapped_color <- function(colour_map = c('lightblue','darkblue'),
                             span = 50) {

  # get color rgb value
  rgb_num <- get_rgb_num(colour_map)
  span <- max(span, length(colour_map))
  # use interpolation to extend colour_map
  col_index <- interpolation(red = rgb_num$red, green = rgb_num$green, blue = rgb_num$blue,
                             span = span)

  col_index
}
