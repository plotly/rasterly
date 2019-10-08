\docType{package}
\name{rasterly-package}
\alias{rasterly}
\alias{rasterly-package}
\title{Easily and rapidly generate raster image data for large datasets using Plotly.js}
\description{
rasterly makes it easy to rapidly generate rasters in R, even for very large datasets, with an aesthetics-based mapping syntax that should be familiar to users of the \link{ggplot2} package. While rasterly does not attempt to reproduce the full functionality of the Datashader graphics pipeline system for Python, the rasterly API has several core elements in common with that software package.

A raster may be described as a matrix of cells or pixels arranged in grid-like fashion, in which each pixel represents a value in the source data.

When combined with the \link{plotly} package and Plotly.js, rasterly enables analysts to generate interactive figures with very large datasets which are responsive enough to embed into Dash for R applications.
}
\usage{
rasterly(data = NULL, mapping = aes(), ..., plot_width = 600,
  plot_height = 600, x_range = NULL, y_range = NULL,
  background = "white", color_map = c("lightblue", "darkblue"),
  color_key = NULL, show_raster = TRUE, drop_data = FALSE,
  variable_check = FALSE)
}
\arguments{
\item{data}{Dataset to use for generating the plot. If not provided, data must be supplied in each layer of the plot.
For best performance, particularly when processing large datasets, use of \link[data.table]{data.table} is recommended.}

\item{mapping}{Default list of aesthetic mappings to use for plot. The same with `ggplot2` \link[ggplot2]{aes}.
See details.}

\item{...}{Other arguments which will be passed through to layers.}

\item{plot_width}{Integer. The width of the image to plot; must be a positive integer. A higher value indicates a higher resolution.}

\item{plot_height}{Integer. The height of the image to plot; must be a positive integer. A higher value indicates a higher resolution.}

\item{x_range}{Vector of type numeric. The range of `x`; it can be used to clip the image. For larger datasets, providing `x_range`
may result in improved performance.}

\item{y_range}{Vector of type numeric. The range of `y`; it can be used to clip the image. For larger datasets, providing `y_range`
may result in improved performance.}

\item{background}{Character. The background color of the image to plot.}

\item{color_map}{Vector of type character. Color(s) used to draw each pixel. The `color_map` is extended by linear interpolation
independently for RGB. The darkness of the mapped color depends upon the values of the aggregation matrix.}

\item{color_key}{Vector of type character. The `color_key` is used for categorical variables; it is passed when the `color` aesthetic
is provided.}

\item{show_raster}{Logical. Should the raster be displayed?}

\item{drop_data}{Logical. When working with large datasets, drops the original data once processed according to the provided
`aes()` parameters, using the `remove()` function. See details for additional information.}

\item{variable_check}{Logical. If `TRUE`, drops unused columns to save memory. Use of this option may result in reduced performance.}
}
\value{
An environment wrapped by a list which defines the properties of the raster data to be generated.
}
\description{
Create a rasterly object, to which aggregation layers may be added. This function is the first step in the process
of generating raster image data using the package. The `rasterly` function is not intended to be used in isolation, since aggregation layers are required for full functionality.
}
\details{
\itemize{
 \item{}{The rasterly package currently supports five aesthetics via `aes()`: "x", "y", "on", "color", and "size".
 The "on" aesthetic specifies the variable upon which the reduction function should be applied to generate the raster data.
 }
 \item{}{`drop_data` can help save space, particularly when large datasets are used. However, dropping the original dataset
 may result in errors when attempting to set or update `aes()` parameters within rasterly layers.
}
}
}
\note{
Calling `rasterly()` without providing `rasterly_...()` layers has no effect.
More info can be found in \href{https://github.com/plotly/rasterly/blob/master/README.md}{README.md}.
}
\examples{
\dontrun{
# Load data
ridesRaw_1 <- "https://raw.githubusercontent.com/plotly/datasets/master/uber-rides-data1.csv" %>%
  data.table::fread(stringsAsFactors = FALSE)
ridesRaw_2 <- "https://raw.githubusercontent.com/plotly/datasets/master/uber-rides-data2.csv" %>%
  data.table::fread(stringsAsFactors = FALSE)
ridesRaw_3 <- "https://raw.githubusercontent.com/plotly/datasets/master/uber-rides-data3.csv"  %>%
  data.table::fread(stringsAsFactors = FALSE)
ridesDf <- list(ridesRaw_1, ridesRaw_2, ridesRaw_3) %>%
  data.table::rbindlist()
  
ridesDf %>%
  rasterly(mapping = aes(x = Lat, y = Lon)) %>%
  rasterize_points() -> p
p  
}
}
\seealso{
\itemize{
  \link{rasterize_points}, \link{rasterly_build}, \link{[.rasterly}, \link{[<-.rasterly}
  \item \url{https://github.com/plotly/rasterly}
  \item \url{https://github.com/plotly/dashR}  
  \item Please report bugs at \url{https://github.com/plotly/rasterly/issues}
}
}