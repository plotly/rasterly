# rasterly
[![Build Status](https://travis-ci.org/z267xu/rasterly.svg?branch=master)](https://travis-ci.org/z267xu/rasterly)

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
Parquet files can provide efficient data compression for large datasets. There are a few options in R for importing Parquet data. One of these is the [`arrow`](https://cran.r-project.org/web/packages/arrow/index.html) package, now available on CRAN.

The package must build Apache Arrow first, so it may take a few minutes to install the first time around.

```
library(arrow)
parquet_data <- read_parquet("somefile.parquet")
# returns a data.frame if sparklyr is not loaded, otherwise it will be a tibble
# to obtain an ordinary data.frame, some slight postprocessing may be required
# parquet_data <- base::as.data.frame(parquet_data)
```

## Installing the package

`rasterly` can be installed directly from github
```
remotes::install_github("https://github.com/plotly/rasterly")
```

## Visualizing data with `rasterly`

`rasterly` is inspired by the [`datashader`](http://datashader.org/getting_started/index.html) package available for Python. Both provide the capability to generate raster data for rapid rendering of graphics for even very large datasets.

In terms of performance, `datashader` is faster but `rasterly` is comparable. `rasterly` aims to provide a user-friendly interface to generate raster data for use with the `plotly` package; it cannot be used for plotting or rendering figures on its own.

#### Producing an interactive graph with the plotly package

To illustrate the basic functionality provided by the package, we'll start by retrieving data on Uber trips taken in New York City from April 1st until September 30th of 2014. The dataset includes 4,533,327 observations, and is several gigabytes in size.

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

Now that the data are loaded, we can pass them to `plot_ly` and pipe the output into `add_rasterly`:

```
plot_ly(ridesDf, x = ~Lat, y = ~Lon) %>%
 add_rasterly()
```
![](man/figures/add_rasterizer.gif)

#### General usage

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
"r" contains image raster and other useful info (like numeric aggregation matrices) required to produce the image but it does **not** provide any graphs.

## Example use in an interactive web application

A sample [Dash for R](https://github.com/plotly/dashR) application to visualize US census data is [available](https://github.com/plotly/rasterly/tree/master/apps/UScensus).
