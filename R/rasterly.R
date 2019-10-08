#' @useDynLib rasterly
#' @import Rcpp
#' @import rlang
#' @import methods
#' @importFrom grDevices rgb col2rgb hcl extendrange as.raster
#' @importFrom magrittr '%>%'
#' @importFrom stats ecdf approx setNames na.omit
#' @importFrom ggplot2 aes
#' @importFrom data.table data.table
#' @importFrom compiler cmpfun
#' @export
rasterly <- function(data = NULL,
                     mapping = aes(),
                     ...,
                     plot_width = 600, plot_height = 600,
                     x_range = NULL, y_range = NULL,
                     background = "white",
                     color_map = c('lightblue','darkblue'),
                     color_key = NULL,
                     show_raster = TRUE,
                     drop_data = FALSE,
                     variable_check = FALSE) {
  mapping <- rename_mapping(mapping)
  
  stopifnot(
    exprs = {
      is.numeric(plot_width) && plot_width > 0
      is.numeric(plot_height) && plot_height > 0
    }
  )
  plot_width <- round(plot_width)
  plot_height <- round(plot_height)
  
  if(!is.null(x_range)) {
    stopifnot(
      exprs = {
        length(x_range) == 2 && is.numeric(x_range)
      }
    )
  }
  
  if(!is.null(y_range)) {
    stopifnot(
      exprs = {
        length(y_range) == 2 && is.numeric(y_range)
      }
    )
  }
  
  if(!is.null(data) && !rlang::is_empty(mapping)) {
    aesthetics <- get_aesthetics(data, mapping, variable_check, ...)
    range <- get_range(x_range = x_range, 
                       y_range = y_range,
                       x = aesthetics$x, 
                       y = aesthetics$y)
    x_range <- range$x_range
    y_range <- range$y_range
    
    if(drop_data) {
      remove(data)
      message("drop_data is TRUE; removing the source dataset. Attempts to pass new aes() parameters to layers may generate an error.")
    }
  }
  
  # make sure all arguments exist in `rasterly` environment
  args <- list(...)
  reduction_func <- args$reduction_func
  layout <- args$layout
  glyph <- args$glyph
  group_by_data_table <- args$group_by_data_table
  
  p <- structure(
    list(
      rasterly_env = environment()
    ),
    class = c("rasterly")
  )
  invisible(return(p))
}
