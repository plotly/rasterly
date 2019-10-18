# rasterly <img src="man/figures/logo.png" align="right" width="120" />
[![Build Status](https://travis-ci.org/z267xu/rasterly.svg?branch=master)](https://travis-ci.org/z267xu/rasterly)
[![Codecov test coverage](https://codecov.io/gh/z267xu/rasterly/branch/master/graph/badge.svg)](https://codecov.io/gh/z267xu/rasterly?branch=master)

Easily and rapidly visualize very large datasets with R and the plotly package.

## Importing large datasets for use with rasterly

`rasterly` is an R package to generate raster data for very large datasets; combined with Plotly.js and the plotly package, it enables analysts to generate interactive figures which are responsive enough to embed into web applications.

There are several ways to import very large datasets into R for use with `rasterly`; one option is the `data.table` package (https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html).

* csv file:
```
library(data.table)
data <- data.table::fread("yourpath/somefile.csv") # or a link
```

* parquet file:
Parquet files can provide efficient data compression for large datasets. Package `reticulate` (https://rstudio.github.io/reticulate/) offers "pandas" library (from Python) in R, which can help load parquet files.
```
library(data.table)
library(reticulate)
library(magrittr)
pandas <- reticulate::import("pandas")
read_parquet <- function(path, columns = NULL) {
  if (!is.null(columns)) columns <- as.list(columns)
  path.expand(path) %>%
      normalizePath() %>%
      pandas$read_parquet(., columns = columns) %>%
      data.table::as.data.table(., stringsAsFactors = FALSE)
}
data <- read_parquet("yourpath/somefile.parquet")
```
Note, make sure `NumPy` and `Pandas` are installed with latest version.

## Install

`rasterly` can be installed directly from github
```
remotes::install_github("https://github.com/plotly/rasterly", ref = "dev")
```

## Visualizing data with `rasterly`

`rasterly` is inspired by the [`datashader`](http://datashader.org/getting_started/index.html) package available for Python. Both provide the capability to generate raster data for rapid rendering of graphics from large datasets.

In terms of performance, `datashader` is faster but `rasterly` is comparable. `rasterly` aims to provide a user-friendly interface to generate single channel heatmaps using the `plotly` package.

#### Basic usage

Data highlights Uber trips taken in New York City from April 1 2014 to September 30 2014 with 4533327 observations.

To illustrate the basic functionality provided by the package, we'll start by retrieving data on Uber trips taken in New York City from April 1st until September 30th of 2014. The dataset includes 4,533,327 observations.

```
# Load data
ridesRaw_1 <- "https://raw.githubusercontent.com/plotly/datasets/master/uber-rides-data1.csv" %>%
  data.table::fread(stringsAsFactors = FALSE)
ridesRaw_2 <- "https://raw.githubusercontent.com/plotly/datasets/master/uber-rides-data2.csv" %>%
  data.table::fread(stringsAsFactors = FALSE)
ridesRaw_3 <- "https://raw.githubusercontent.com/plotly/datasets/master/uber-rides-data3.csv"  %>%
  data.table::fread(stringsAsFactors = FALSE)
ridesDf <- list(ridesRaw_1, ridesRaw_2, ridesRaw_3) %>%
  data.table::rbindlist()
```

Pass the data into `rasterly`:
```
ridesDf %>%
  rasterly(mapping = aes(x = Lat, y = Lon)) %>%
  rasterize_points() -> p
p
```
![](man/figures/grid_rasterizer.png)

Note that, "p" is a list of environments. The display info can be accessed through
```
r <- rasterly_build(p)
str(r)
```
"r" contains image raster and other useful info (like numeric aggregation matrices) to produce image but it does **not** provide any graphs.

#### Static graph

* `grid`
```
# same with plot(p)
p
```

#### Interactive graph

* `plotly`
```
plot_ly(ridesDf, x = ~Lat, y = ~Lon) %>%
 add_rasterly_heatmap()
```
![](man/figures/add_rasterizer.gif)

## Apps

A sample [Dash for R](https://github.com/plotly/dashR) application to visualize US census data is [available](https://github.com/plotly/rasterly/tree/master/apps/UScensus).
