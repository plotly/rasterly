# rasterizer

Easily and rapidly visualize very large datasets with R and the plotly package.

## Load large data in R

`rasterizer` is an R package to generate plots using very large datasets in seconds. 

There are several ways to import very large datasets into R for use with `rasterizer`; one option is the `data.table` package (https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html).

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

`rasterizer` can be installed directly from github
```
remotes::install_github("https://github.com/plotly/rasterizer", ref = "dev")
```

## Use `rasterizer` to display large data set

`rasterizer` is built based on `datashader` http://datashader.org/getting_started/index.html in python. Both are designed by "rasterizing" large data set into images. In computation, `datashader` is faster but `rasterizer` is comparable; in usage, `rasterizer` provides more readable and flexible operation interface.

#### Basic usage

Data highlights Uber trips taken in New York City from April 1 2014 to September 30 2014 with 4533327 observations.
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

Start `rasterizer`
```
ridesDf %>%
  rasterizer(mapping = aes(x = Lat, y = Lon)) %>% 
  rasterize_points() -> p
p
```
![](man/figures/grid_rasterizer.png)

Note that, "p" is a list of environments. The display info can be accessed through
```
r <- rasterizer_build(p)
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
 add_rasterizer()
```
![](man/figures/add_rasterizer.gif)

## Apps

USA census app built by `dashR` can be found in https://github.com/plotly/rasterizer/tree/master/apps/UScensus
