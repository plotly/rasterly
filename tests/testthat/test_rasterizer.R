context("test examples")
library(data.table)
library(magrittr)
library(grid)
library(rasterizer)

data <- data.table::data.table(x = rnorm(1e5), y = rnorm(1e5), z = sample(1:5, 1e5, replace = TRUE))
min_x <- min(data$x)
max_x <- max(data$x)
min_y <- min(data$y)
max_y <- max(data$y)
colours <- c("#FF0000","#FF3F00","#FF7F00","#FFBF00","#FFFF00","#BFFF00","#7FFF00","#3FFF00",
            "#00FF00","#00FF3F","#00FF7F","#00FFBF","#00FFFF","#00BFFF","#007FFF","#003FFF",
            "#0000FF","#3F00FF","#7F00FF","#BF00FF","#FF00FF","#FF00BF","#FF007F","#FF003F")
test_that("example works", {
  # ex1
  ########### "sum" ##############
  rasterizer(data,
             mapping = aes(x = x, y = y),
             x_range = c(min_x, max_x),
             y_range = c(min_y, max_y)) %>%
    rasterize_points(xlim = c(min_x, (max_x + min_x)/2),
                     ylim = c(min_y, (max_y + min_y)/2),
                     colour_map = fire) %>% 
    rasterize_points(mapping = aes(x = x, y = y, on = -x),
                     xlim = c((max_x + min_x)/2, max_x),
                     ylim = c(min_y, (max_y + min_y)/2),
                     colour_map = c("lightblue", "darkblue")) %>% 
    rasterize_points(mapping = aes(x = x, y = y, colour = z),
                     xlim = c((max_x + min_x)/2, max_x),
                     ylim = c((max_y + min_y)/2, max_y),
                     colour_key = rev(colours)) %>% 
    rasterize_points(mapping = aes(x = x, y = y, colour = z, size = z),
                     xlim = c(min_x, (max_x + min_x)/2),
                     ylim = c((max_y + min_y)/2, max_y),
                     max_size = 3,
                     layout = "cover") %>%
    rasterizer_build() -> ds
  expect_equal(grid::is.grob(grid::rasterGrob(ds$image)), TRUE)
  
  ########### "any" ##############
  # reduction function is any and group_by_data_table is TRUE
  rasterizer(data,
             mapping = aes(x = x, y = y, on = x),
             background = "black",
             x_range = c(min_x, max_x),
             y_range = c(min_y, max_y),
             colour_key = colours,
             reduction_func = "any") %>%
    rasterize_points(colour_map = fire, group_by_data_table = FALSE) %>%
    rasterizer_build() -> ds
  expect_equal(is.rasterizer(ds), TRUE)
  
  # reduction function is any and group_by_data_table is FALSE
  rasterizer(data,
             mapping = aes(x = x, y = y, on = x),
             background = "black",
             x_range = c(min_x, max_x),
             y_range = c(min_y, max_y),
             colour_key = colours,
             reduction_func = "any") %>%
    rasterize_points(colour_map = fire, group_by_data_table = FALSE) %>%
    rasterizer_build() -> ds
  expect_equal(is.rasterizer(ds), TRUE)
  
  ########### "mean" ##############
  # reduction function is any and group_by_data_table is TRUE
  rasterizer(data,
             mapping = aes(x = x, y = y, on = x),
             background = "black",
             x_range = c(min_x, max_x),
             y_range = c(min_y, max_y),
             colour_key = colours,
             reduction_func = "mean") %>%
    rasterize_points(colour_map = fire, group_by_data_table = FALSE) %>%
    rasterizer_build() -> ds
  expect_equal(is.rasterizer(ds), TRUE)
  
  # reduction function is any and group_by_data_table is FALSE
  rasterizer(data,
             mapping = aes(x = x, y = y, on = x),
             background = "black",
             x_range = c(min_x, max_x),
             y_range = c(min_y, max_y),
             colour_key = colours,
             reduction_func = "mean") %>%
    rasterize_points(colour_map = fire, group_by_data_table = FALSE) %>%
    rasterizer_build() -> ds
  expect_equal(is.rasterizer(ds), TRUE)
  
  ########### "first" ##############
  rasterizer(data,
             mapping = aes(x = x, y = y, on = x),
             background = "black",
             x_range = c(min_x, max_x),
             y_range = c(min_y, max_y),
             colour_key = colours,
             reduction_func = "first") %>%
    rasterize_points(colour_map = fire, group_by_data_table = FALSE) %>%
    rasterizer_build() -> ds
  expect_equal(is.rasterizer(ds), TRUE)
  
  # reduction function is any and group_by_data_table is FALSE
  rasterizer(data,
             mapping = aes(x = x, y = y, on = x),
             background = "black",
             x_range = c(min_x, max_x),
             y_range = c(min_y, max_y),
             colour_key = colours,
             reduction_func = "first") %>%
    rasterize_points(colour_map = fire, group_by_data_table = FALSE) %>%
    rasterizer_build() -> ds
  expect_equal(is.rasterizer(ds), TRUE)
  
  ########### "last" ##############
  rasterizer(data,
             mapping = aes(x = x, y = y, on = x),
             background = "black",
             x_range = c(min_x, max_x),
             y_range = c(min_y, max_y),
             colour_key = colours,
             reduction_func = "last") %>%
    rasterize_points(colour_map = fire, group_by_data_table = FALSE) %>%
    rasterizer_build() -> ds
  expect_equal(is.rasterizer(ds), TRUE)
  
  # reduction function is any and group_by_data_table is FALSE
  rasterizer(data,
             mapping = aes(x = x, y = y, on = x),
             background = "black",
             x_range = c(min_x, max_x),
             y_range = c(min_y, max_y),
             colour_key = colours,
             reduction_func = "last") %>%
    rasterize_points(colour_map = fire, group_by_data_table = FALSE) %>%
    rasterizer_build() -> ds
  expect_equal(is.rasterizer(ds), TRUE)
  
  ########### "m2" ##############
  rasterizer(data,
             mapping = aes(x = x, y = y, on = x),
             background = "black",
             x_range = c(min_x, max_x),
             y_range = c(min_y, max_y),
             colour_key = colours,
             reduction_func = "m2") %>%
    rasterize_points(colour_map = fire, group_by_data_table = FALSE) %>%
    rasterizer_build() -> ds
  expect_equal(is.rasterizer(ds), TRUE)
  
  # reduction function is any and group_by_data_table is FALSE
  rasterizer(data,
             mapping = aes(x = x, y = y, on = x),
             background = "black",
             x_range = c(min_x, max_x),
             y_range = c(min_y, max_y),
             colour_key = colours,
             reduction_func = "m2") %>%
    rasterize_points(colour_map = fire, group_by_data_table = FALSE) %>%
    rasterizer_build() -> ds
  expect_equal(is.rasterizer(ds), TRUE)
  
  ########### "max" ##############
  rasterizer(data,
             mapping = aes(x = x, y = y, on = x),
             background = "black",
             x_range = c(min_x, max_x),
             y_range = c(min_y, max_y),
             colour_key = colours,
             reduction_func = "max") %>%
    rasterize_points(colour_map = fire, group_by_data_table = FALSE) %>%
    rasterizer_build() -> ds
  expect_equal(is.rasterizer(ds), TRUE)
  
  # reduction function is any and group_by_data_table is FALSE
  rasterizer(data,
             mapping = aes(x = x, y = y, on = x),
             background = "black",
             x_range = c(min_x, max_x),
             y_range = c(min_y, max_y),
             colour_key = colours,
             reduction_func = "max") %>%
    rasterize_points(colour_map = fire, group_by_data_table = FALSE) %>%
    rasterizer_build() -> ds
  expect_equal(is.rasterizer(ds), TRUE)
})
