context("test examples")
library(data.table)
library(magrittr)
library(grid)
library(rasterizer)

nyc <- data.table::fread("testdata/nyc_taxi.csv")

colors <- c("#FF0000","#FF3F00","#FF7F00","#FFBF00","#FFFF00","#BFFF00","#7FFF00","#3FFF00",
            "#00FF00","#00FF3F","#00FF7F","#00FFBF","#00FFFF","#00BFFF","#007FFF","#003FFF",
            "#0000FF","#3F00FF","#7F00FF","#BF00FF","#FF00FF","#FF00BF","#FF007F","#FF003F")

colors <- colors[unique(nyc$pick_up_hour) + 1]
max_x <- max(nyc$pickup_x)
min_x <- min(nyc$pickup_x)
max_y <- max(nyc$pickup_y)
min_y <- min(nyc$pickup_y)

test_that("example works", {
  # ex1
  canvas(nyc,
         mapping = aes(x = pickup_x, y = pickup_y),
         background = "black",
         x_range = c(min_x, max_x),
         y_range = c(min_y, max_y)) %>%
    aggregation_points(xlim = c(min_x, (max_x + min_x)/2),
                       ylim = c(min_y, (max_y + min_y)/2),
                       colour_map = fire) %>% 
    aggregation_points(xlim = c((max_x + min_x)/2, max_x),
                       ylim = c(min_y, (max_y + min_y)/2),
                       colour_map = c("lightblue", "darkblue")) %>% 
    aggregation_points(mapping = aes(x = pickup_x, y = pickup_y, colour = pick_up_hour),
                       xlim = c((max_x + min_x)/2, max_x),
                       ylim = c((max_y + min_y)/2, max_y),
                       colour_key = colors) %>% 
    aggregation_points(mapping = aes(x = pickup_x, y = pickup_y, colour = pick_up_hour, size = passenger_count),
                       xlim = c(min_x, (max_x + min_x)/2),
                       ylim = c((max_y + min_y)/2, max_y),
                       max_size = 3,
                       layout = "cover") %>%
    rasterizer() -> ds
  expect_equal(grid::is.grob(grid::rasterGrob(ds$image)), TRUE)
  
  # reduction function is any and group_by_data_table is TRUE
  canvas(nyc,
         mapping = aes(x = pickup_x, y = pickup_y, on = pickup_x),
         background = "black",
         x_range = c(min_x, max_x),
         y_range = c(min_y, max_y),
         colour_key = colors,
         reduction_func = "any") %>%
    aggregation_points(colour_map = fire, group_by_data_table = FALSE) %>%
    rasterizer() -> ds
  expect_equal(is.rasterizer(ds), TRUE)
  
  # reduction function is any and group_by_data_table is FALSE
  canvas(nyc,
         mapping = aes(x = pickup_x, y = pickup_y, on = pickup_x),
         background = "black",
         x_range = c(min_x, max_x),
         y_range = c(min_y, max_y),
         colour_key = colors,
         reduction_func = "any") %>%
    aggregation_points(colour_map = fire, group_by_data_table = FALSE) %>%
    rasterizer() -> ds
  expect_equal(is.rasterizer(ds), TRUE)
})
