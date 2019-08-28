get_rgb_num <- function(colour) {
  rgb_num <- grDevices::col2rgb(colour)
  list(
    red = rgb_num['red', ],
    green = rgb_num['green', ],
    blue = rgb_num['blue', ]
  )
}

interpolation <- function(..., span = NULL) {

  if(is.null(span)) span <- 50 # default setting in approx
  args <- list(...)
  lapply(args,
         function(arg){
           stats::approx(x = arg, n = span)[['y']]
         })

}

gg_colour_hue <- function(n) {
  hues <- seq(15, 375, length = n + 1)
  grDevices::hcl(h = hues, l = 65, c = 100)[1:n]
}
