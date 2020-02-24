## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
    echo = TRUE, 
    tidy.opts = list(width.cutoff = 65),
    tidy = TRUE)

set.seed(12314159)

imageDirectory <- "./images/introduction"
path_concat <- function(path1, path2, sep="/") {paste(path1, path2, sep = sep)}

## ----library, eval = TRUE, echo = TRUE, fig.align="center", fig.width = 6, fig.height = 4, out.width = "75%", warning=FALSE, message=FALSE----
library(rasterly)
library(data.table)
library(lubridate)
library(grid)
library(plotly)

## ----data, eval = FALSE, echo = TRUE, fig.align="center", fig.width = 6, fig.height = 5, out.width = "75%", warning=FALSE, message=FALSE, tidy=FALSE----
#  # Load data
#  ridesRaw_1 <- "https://raw.githubusercontent.com/plotly/datasets/master/uber-rides-data1.csv" %>%
#    data.table::fread(stringsAsFactors = FALSE)
#  ridesRaw_2 <- "https://raw.githubusercontent.com/plotly/datasets/master/uber-rides-data2.csv" %>%
#    data.table::fread(stringsAsFactors = FALSE)
#  ridesRaw_3 <- "https://raw.githubusercontent.com/plotly/datasets/master/uber-rides-data3.csv"  %>%
#    data.table::fread(stringsAsFactors = FALSE)
#  ridesDf <- list(ridesRaw_1, ridesRaw_2, ridesRaw_3) %>%
#    data.table::rbindlist()
#  
#  # Extract hour of trip taken
#  time <- lubridate::ymd_hms(ridesDf$`Date/Time`)
#  ridesDf <-  ridesDf[, 'Date/Time':=NULL][, list(Lat,
#                                                  Lon,
#                                                  hour = lubridate::hour(time),
#                                                  month = lubridate::month(time),
#                                                  day = lubridate::day(time))]

## ----basic, eval = FALSE, echo = TRUE, fig.align="center", fig.width = 6, fig.height = 5, out.width = "75%", warning=FALSE, message=FALSE, tidy=FALSE----
#  start_time <- Sys.time()
#  p <- ridesDf %>%
#    rasterly(mapping = aes(x = Lat, y = Lon)) %>%
#    rasterly_points()
#  p
#  end_time <- Sys.time()
#  end_time - start_time

## ---- out.width= "60%", fig.align="center", echo=FALSE------------------------
knitr::include_graphics(path_concat(imageDirectory, "uberBasic.PNG"))

## -----------------------------------------------------------------------------
image <- as.raster(matrix((1:4)/4, nrow = 2))
image
# mapping this image onto a 1 <= x <= 2 and 2 <= y <= 5 plane
image2data(image, x_range = c(1,2), y_range = c(2,5))

## ----basic plot, eval = FALSE, echo = TRUE, fig.align="center", fig.width = 6, fig.height = 5, out.width = "75%", warning=FALSE, message=FALSE, tidy=FALSE----
#  imageData <- image2data(p)
#  # basic graphics
#  # It is slow but still much faster than drawing the huge data directly)
#  plot(x = imageData$x, y = imageData$y, col = imageData$color)

## ---- out.width= "60%", fig.align="center", echo=FALSE------------------------
knitr::include_graphics(path_concat(imageDirectory, "image2data.PNG"))

## ----subsetting, eval = FALSE, echo = TRUE, fig.align="center", fig.width = 6, fig.height = 5, out.width = "75%", warning=FALSE, message=FALSE, tidy=FALSE----
#  p["background"]
#  # $rasterly_env
#  # [1] "white"
#  
#  # $rasterlyPoints1
#  # [1] "white"
#  ########### Replace the background in child layer `rasterly_points()`
#  p["background", level = 2] <- "black"
#  p["background"]
#  # $rasterly_env
#  # [1] "white"
#  
#  # $rasterlyPoints1
#  # [1] "black"
#  ########## Colors in both `rasterly()` and `rasterly_points()` are replaced
#  ## fire is a vector of colors (as character strings) with length 256
#  ## see `rasterly::fire`
#  p["color", level = 1:2] <- fire_map
#  p

## ---- out.width= "60%", fig.align="center", echo=FALSE------------------------
knitr::include_graphics(path_concat(imageDirectory, "darkBg.PNG"))

## ----rasterly_build, eval = FALSE, echo = TRUE, fig.align="center", fig.width = 6, fig.height = 5, out.width = "75%", warning=FALSE, message=FALSE, tidy=FALSE----
#  build <- rasterly_build(p)
#  str(build)

## ----add_rasterly_heatmap, eval = FALSE, echo = TRUE, fig.align="center", fig.width = 6, fig.height = 5, out.width = "75%", warning=FALSE, message=FALSE, tidy=FALSE----
#  plotly::plot_ly(ridesDf, x = ~Lat, y = ~Lon) %>%
#    add_rasterly_heatmap() %>%
#    layout(
#      title = "Uber drives",
#      xaxis = list(
#        title = "Lat"
#      ),
#      yaxis = list(
#        title = "Lon"
#      )
#    )

## ---- out.width= "60%", fig.align="center", echo=FALSE------------------------
knitr::include_graphics(path_concat(imageDirectory, "add_rasterizer.gif"))

## ----plotly_rasterly, eval = FALSE, echo = TRUE, fig.align="center", fig.width = 6, fig.height = 5, out.width = "75%", warning=FALSE, message=FALSE, tidy=FALSE----
#  # if as_image is FALSE, the image will be transformed to a data.frame
#  plotRasterly(ridesDf,
#               mapping = aes(x = Lat, y = Lon),
#               as_image = TRUE)

## ----ggRasterly, eval = FALSE, echo = TRUE, fig.align="center", fig.width = 6, fig.height = 5, out.width = "75%", warning=FALSE, message=FALSE, tidy=FALSE----
#  ggRasterly(data = ridesDf,
#             mapping = aes(x = Lat, y = Lon, color = hour),
#             color = hourColors_map) +
#    labs(title = "New York Uber",
#         subtitle = "Apr to Sept, 2014",
#         caption = "Data from https://raw.githubusercontent.com/plotly/datasets/master")

## ---- out.width= "60%", fig.align="center", echo=FALSE------------------------
knitr::include_graphics(path_concat(imageDirectory, "ggUber.PNG"))

## ----API, eval = FALSE, echo = TRUE, fig.align="center", fig.width = 6, fig.height = 5, out.width = "75%", warning=FALSE, message=FALSE, tidy=FALSE----
#  r <- rasterly(data = ridesDf,
#                  mapping = aes(x = Lat, y = Lon))

## ----set color, eval = FALSE, echo = TRUE, fig.align="center", fig.width = 6, fig.height = 5, out.width = "75%", warning=FALSE, message=FALSE, tidy=FALSE----
#  r %>%
#    rasterly_points(
#      mapping = aes(color = hour),
#      color = hourColors_map,
#      background = "black"
#    ) -> g
#  g

## ---- out.width= "60%", fig.align="center", echo=FALSE------------------------
knitr::include_graphics(path_concat(imageDirectory, "uberColor.PNG"))

## ----set color cover, eval = FALSE, echo = TRUE, fig.align="center", fig.width = 6, fig.height = 5, out.width = "75%", warning=FALSE, message=FALSE, tidy=FALSE----
#  r %>%
#    rasterly_points(
#      mapping = aes(color = hour),
#      color = hourColors_map,
#      background = "black",
#      layout = "cover"
#    )

## ---- out.width= "60%", fig.align="center", echo=FALSE------------------------
knitr::include_graphics(path_concat(imageDirectory, "uberColorCover.PNG"))

## ----set on, eval = FALSE, echo = TRUE, fig.align="center", fig.width = 6, fig.height = 5, out.width = "75%", warning=FALSE, message=FALSE, tidy=FALSE----
#  r %>%
#    rasterly_points(
#      # take the "mean" reduction function
#      # more details are in section 'Reduction function'
#      reduction_func = "mean",
#      mapping = aes(on = -Lat)
#    )

## ----set size, eval = FALSE, echo = TRUE, fig.align="center", fig.width = 6, fig.height = 5, out.width = "75%", warning=FALSE, message=FALSE, tidy=FALSE----
#  r %>%
#    rasterly_points(
#      mapping = aes(size = month),
#      max_size = 4
#    )

## ----reduction on mean, eval = FALSE, echo = TRUE, fig.align="center", fig.width = 6, fig.height = 5, out.width = "75%", warning=FALSE, message=FALSE, tidy=FALSE----
#  r %>%
#    rasterly_points(
#      reduction_func = "mean", # process the data points using the mean reduction function
#      background = "black",    # change background to "black" from right to left (from dark to light)
#      color = fire_map # provide a custom color_map
#    )

## ---- out.width= "60%", fig.align="center", echo=FALSE------------------------
knitr::include_graphics(path_concat(imageDirectory, "meanAgg.PNG"))

## ----reduction on any, eval = FALSE, echo = TRUE, fig.align="center", fig.width = 6, fig.height = 5, out.width = "75%", warning=FALSE, message=FALSE, tidy=FALSE----
#  r %>%
#    rasterly_points(
#      reduction_func = "any",
#      color = c("white", "black")
#    )

## ---- out.width= "60%", fig.align="center", echo=FALSE------------------------
knitr::include_graphics(path_concat(imageDirectory, "anyAgg.PNG"))

