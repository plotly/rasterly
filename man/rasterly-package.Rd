\docType{package}
\name{rasterly-package}
\alias{rasterly-package}
\title{Easily and rapidly generate raster image data for large datasets using Plotly.js}
\description{
\if{html}{\figure{logo.png}{options: width=175 align='right'}}

rasterly makes it easy to rapidly generate rasters in R, even for very large datasets, with an aesthetics-based mapping syntax that should be familiar to users of the \link{ggplot2} package. While rasterly does not attempt to reproduce the full functionality of the Datashader graphics pipeline system for Python, the rasterly API has several core elements in common with that software package.

A raster may be described as a matrix of cells or pixels arranged in grid-like fashion, in which each pixel represents a value in the source data.

When combined with the \link{plotly} package and Plotly.js, rasterly enables analysts to generate interactive figures with very large datasets which are responsive enough to embed into Dash for R applications.
}

\note{
Calling `rasterly()` without providing `rasterly_...()` layers has no effect.
More info can be found in \href{https://github.com/plotly/rasterly/blob/master/README.md}{README.md}
}

\seealso{
\itemize{
  \link{rasterize_points}, \link{rasterly_build}, \link{[.rasterly}, \link{[<-.rasterly}
  \item \url{https://github.com/plotly/rasterly}
  \item \url{https://github.com/plotly/dashR}  
  \item Please report bugs at \url{https://github.com/plotly/rasterly/issues}
}
}

\author{
\strong{Maintainer}: Zehao Xu \email{z267xu@uwaterloo.ca}

Authors:
\itemize{
  \item Zehao Xu \email{z267xu@uwaterloo.ca}
  \item Ryan Patrick Kyle \email{ryan@plot.ly}
}

Other contributors:
\itemize{
  \item  Plotly Technologies [copyright holder]
}
}
