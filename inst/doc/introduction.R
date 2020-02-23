## ----data----------------------------------------------------------------
library(rasterly)
library(data.table)
library(lubridate)
library(grid)
library(plotly)

# Load data
ridesRaw_1 <- "https://raw.githubusercontent.com/plotly/datasets/master/uber-rides-data1.csv" %>%
  data.table::fread(stringsAsFactors = FALSE)
ridesRaw_2 <- "https://raw.githubusercontent.com/plotly/datasets/master/uber-rides-data2.csv" %>% 
  data.table::fread(stringsAsFactors = FALSE)
ridesRaw_3 <- "https://raw.githubusercontent.com/plotly/datasets/master/uber-rides-data3.csv"  %>% 
  data.table::fread(stringsAsFactors = FALSE)
ridesDf <- list(ridesRaw_1, ridesRaw_2, ridesRaw_3) %>% 
  data.table::rbindlist()

# Extract hour of trip taken
time <- lubridate::ymd_hms(ridesDf$`Date/Time`)
ridesDf <-  ridesDf[, 'Date/Time':=NULL][, list(Lat, 
                                                Lon,
                                                hour = lubridate::hour(time), 
                                                month = lubridate::month(time),
                                                day = lubridate::day(time))]
head(ridesDf)

## ----basic, warning=FALSE, message=FALSE, fig.width = 4, fig.height = 3----
start_time <- Sys.time()
p <- ridesDf %>% 
  rasterly(mapping = aes(x = Lat, y = Lon)) %>% 
  rasterly_points()
p
end_time <- Sys.time()
end_time - start_time

## ------------------------------------------------------------------------
image <- as.raster(matrix((1:4)/4, nrow = 2))
image
# mapping this image onto a 1 <= x <= 2 and 2 <= y <= 5 plane
image2data(image, x_range = c(1,2), y_range = c(2,5))

## ----basic plot, warning=FALSE, message=FALSE, fig.width = 4, fig.height = 3, eval = FALSE----
#  imageData <- image2data(p)
#  # basic graphics
#  # It is slow but still much faster than drawing the huge data directly)
#  plot(x = imageData$x, y = imageData$y, col = imageData$color)

## ----list return---------------------------------------------------------
# A list of environments
str(p)

## ----subsetting, warning=FALSE, message=FALSE, fig.width = 4, fig.height = 3----
p["background"]
# Replace the background in child layer `rasterly_points()`
p["background", level = 2] <- "black"
p["background"]
# colors in both `rasterly()` and `rasterly_points()` are replaced
## fire is a vector of colors (as character strings) with length 256
## see `rasterly::fire`
p["color", level = 1:2] <- fire_map
p

## ----rasterly_build------------------------------------------------------
build <- rasterly_build(p)
str(build)

## ----add_rasterly_heatmap, fig.width = 6, fig.height = 5-----------------
plotly::plot_ly(ridesDf, x = ~Lat, y = ~Lon) %>%
  add_rasterly_heatmap() %>% 
  layout(
    title = "Uber drives",
    xaxis = list(
      title = "Lat"
    ),
    yaxis = list(
      title = "Lon"
    )
  )

## ----plotly_rasterly, warning=FALSE, message=FALSE, fig.width = 4, fig.height = 3, eval = FALSE----
#  # if as_image is FALSE, the image will be transformed to a data.frame
#  plotRasterly(ridesDf,
#               mapping = aes(x = Lat, y = Lon),
#               as_image = TRUE)

## ----ggRasterly, warning=FALSE, message=FALSE, fig.width = 6, fig.height = 5----
ggRasterly(data = ridesDf, 
           mapping = aes(x = Lat, y = Lon, color = hour),
           color = hourColors_map) + 
  labs(title = "New York Uber",
       subtitle = "Apr to Sept, 2014",
       caption = "Data from https://raw.githubusercontent.com/plotly/datasets/master")

## ------------------------------------------------------------------------
r <- rasterly(data = ridesDf, 
                mapping = aes(x = Lat, y = Lon))

## ----set color, fig.width = 4, fig.height = 3----------------------------
r %>% 
  rasterly_points(
    mapping = aes(color = hour),
    color = hourColors_map,
    background = "black"
  ) -> g
g

## ----legend, fig.width = 4, fig.height = 3-------------------------------
    # rasterly doesn't currently support legends, though this feature is forthcoming
    plot(1:24, y = rep(1,24), col = hourColors_map, pch = 19, cex = 3)

## ----number of aggregation matrices--------------------------------------
build_g <- rasterly_build(g)
# the object has only one layer, so we index into the first element
length(build_g$agg[[1]])

## ----set color cover, fig.width = 4, fig.height = 3, eval = FALSE--------
#  r %>%
#    rasterly_points(
#      mapping = aes(color = hour),
#      color = hourColors_map,
#      background = "black",
#      layout = "cover"
#    )

## ----set on, fig.width = 4, fig.height = 3, eval = FALSE-----------------
#  r %>%
#    rasterly_points(
#      reduction_func = "mean", # take the "mean" reduction function
#      mapping = aes(on = -Lat)
#    )

## ----set size, fig.width = 4, fig.height = 3, eval = FALSE---------------
#  r %>%
#    rasterly_points(
#      mapping = aes(size = month),
#      max_size = 4
#    )

## ----reduction on mean, fig.width = 4, fig.height = 3, eval = FALSE------
#  r %>%
#    rasterly_points(
#      reduction_func = "mean", # process the data points using the mean reduction function
#      background = "black",    # change background to "black" from right to left (from dark to light)
#      color = fire_map # provide a custom color_map
#    )

## ----reduction on any, fig.width = 4, fig.height = 3, eval = FALSE-------
#  r %>%
#    rasterly_points(
#      reduction_func = "any",
#      color = c("white", "black")
#    )

