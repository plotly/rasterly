#' Easily and rapidly generate raster image data with support for Plotly.js 
#' @description Create a rasterly object, to which aggregation layers may be added. This function is the first step in the process
#' of generating raster image data using the `rasterly` package.
#' @param data Dataset to use for generating the plot. If not provided, data must be supplied in each layer of the plot.
#' For best performance, particularly when processing large datasets, use of \link[data.table]{data.table} is recommended.
#' @param mapping Default list of aesthetic mappings to use for plot. The same with `ggplot2` \link[ggplot2]{aes}.
#' See details.
#' @param ... Other arguments which will be passed through to layers.
#' @param plot_width Integer. The width of the image to plot; must be a positive integer. A higher value indicates a higher resolution.
#' @param plot_height Integer. The height of the image to plot; must be a positive integer. A higher value indicates a higher resolution.
#' @param x_range Vector of type numeric. The range of `x`; it can be used to clip the image. For larger datasets, providing `x_range`
#' may result in improved performance.
#' @param y_range Vector of type numeric. The range of `y`; it can be used to clip the image. For larger datasets, providing `y_range`
#' may result in improved performance.
#' @param background Character. The background color of the image to plot.
#' @param color Vector of type character. It will determine this color vector is a `color_map` or `color_key` automatically.
#' \itemize{
#'  \item{color_map}{It has Color(s) used to draw each pixel. The `color_map` is extended by linear interpolation
#' independently for RGB. The darkness of the mapped color depends upon the values of the aggregation matrix.
#'  }
#'  \item{color_key}{Vector of type character. The `color_key` is used for categorical variables; 
#'  it is passed when the `color` aesthetic is provided.
#' }
#' }
#' @param show_raster Logical. Should the raster be displayed?
#' @param drop_data Logical. When working with large datasets, drops the original data once processed according to the provided
#' `aes()` parameters, using the `remove()` function. See details for additional information.
#' @param variable_check Logical. If `TRUE`, drops unused columns to save memory; may result in reduced performance.
#' 
#' @return An environment wrapped by a list
#' 
#' @note Calling `rasterly()` without providing `rasterly_...()` layers has no effect.
#' More info can be found in \href{https://github.com/plotly/rasterly/blob/master/README.md}{README.md}
#'
#' @seealso \link{rasterly_points}, \link{rasterly_build}, \link{[.rasterly}, \link{[<-.rasterly}
#' \link{ggRasterly}, \link{plotRasterly}
#' @details 
#' \itemize{
#'  \item{}{The rasterly package currently supports five aesthetics via `aes()`: "x", "y", "on", "color", and "size".
#'  The "on" aesthetic specifies the variable upon which the reduction function should be applied to generate the raster data.
#'  }
#'  \item{}{`drop_data` can help save space, particularly when large datasets are used. However, dropping the original dataset
#'  may result in errors when attempting to set or update `aes()` parameters within rasterly layers.
#' }
#' }
#'
