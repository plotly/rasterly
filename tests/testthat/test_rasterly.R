context("test examples")
library(data.table)
library(magrittr)
library(grid)
library(plotly)
library(rasterly)

data <- data.table::data.table(x = rnorm(1e5), y = rnorm(1e5), z = sample(1:5, 1e5, replace = TRUE))
min_x <- min(data$x)
max_x <- max(data$x)
min_y <- min(data$y)
max_y <- max(data$y)
colors <- c("#FF0000","#FF3F00","#FF7F00","#FFBF00","#FFFF00","#BFFF00","#7FFF00","#3FFF00",
            "#00FF00","#00FF3F","#00FF7F","#00FFBF","#00FFFF","#00BFFF","#007FFF","#003FFF",
            "#0000FF","#3F00FF","#7F00FF","#BF00FF","#FF00FF","#FF00BF","#FF007F","#FF003F")
test_that("example works", {
  # ex1
  ########### "sum" ##############
  rasterly(data,
           mapping = aes(x = x, y = y),
           x_range = c(min_x, max_x),
           y_range = c(min_y, max_y)) %>%
    rasterly_points(xlim = c(min_x, (max_x + min_x)/2),
                     ylim = c(min_y, (max_y + min_y)/2),
                     color = fire) %>% 
    rasterly_points(mapping = aes(x = x, y = y, on = -x),
                     xlim = c((max_x + min_x)/2, max_x),
                     ylim = c(min_y, (max_y + min_y)/2),
                     color = c("lightblue", "darkblue")) %>% 
    rasterly_points(mapping = aes(x = x, y = y, color = z),
                     xlim = c((max_x + min_x)/2, max_x),
                     ylim = c((max_y + min_y)/2, max_y),
                     color = rev(colors)) %>% 
    rasterly_points(mapping = aes(x = x, y = y, color = z, size = z),
                     xlim = c(min_x, (max_x + min_x)/2),
                     ylim = c((max_y + min_y)/2, max_y),
                     max_size = 3,
                     layout = "cover") %>%
    rasterly_build() -> ds
  expect_equal(grid::is.grob(grid::rasterGrob(ds$image)), TRUE)
  
  ########### "any" ##############
  # reduction function is any and group_by_data_table is TRUE
  rasterly(data,
           mapping = aes(x = x, y = y, on = x),
           background = "black",
           x_range = c(min_x, max_x),
           y_range = c(min_y, max_y),
           color = colors,
           reduction_func = "any") %>%
    rasterly_points(color = fire, group_by_data_table = FALSE) %>%
    rasterly_build() -> ds
  expect_equal(is.rasterly(ds), TRUE)
  
  # reduction function is any and group_by_data_table is FALSE
  rasterly(data,
           mapping = aes(x = x, y = y, on = x),
           background = "black",
           x_range = c(min_x, max_x),
           y_range = c(min_y, max_y),
           color = colors,
           reduction_func = "any") %>%
    rasterly_points(color = fire, group_by_data_table = FALSE) %>%
    rasterly_build() -> ds
  expect_equal(is.rasterly(ds), TRUE)
  
  ########### "mean" ##############
  # reduction function is any and group_by_data_table is TRUE
  rasterly(data,
           mapping = aes(x = x, y = y, on = x),
           background = "black",
           x_range = c(min_x, max_x),
           y_range = c(min_y, max_y),
           color = colors,
           reduction_func = "mean") %>%
    rasterly_points(color = fire, group_by_data_table = FALSE) %>%
    rasterly_build() -> ds
  expect_equal(is.rasterly(ds), TRUE)
  
  # reduction function is any and group_by_data_table is FALSE
  rasterly(data,
           mapping = aes(x = x, y = y, on = x),
           background = "black",
           x_range = c(min_x, max_x),
           y_range = c(min_y, max_y),
           color = colors,
           reduction_func = "mean") %>%
    rasterly_points(color = fire, group_by_data_table = FALSE) %>%
    rasterly_build() -> ds
  expect_equal(is.rasterly(ds), TRUE)
  
  ########### "first" ##############
  rasterly(data,
           mapping = aes(x = x, y = y, on = x),
           background = "black",
           x_range = c(min_x, max_x),
           y_range = c(min_y, max_y),
           color = colors,
           reduction_func = "first") %>%
    rasterly_points(color = fire, group_by_data_table = FALSE) %>%
    rasterly_build() -> ds
  expect_equal(is.rasterly(ds), TRUE)
  
  # reduction function is any and group_by_data_table is FALSE
  rasterly(data,
           mapping = aes(x = x, y = y, on = x),
           background = "black",
           x_range = c(min_x, max_x),
           y_range = c(min_y, max_y),
           color = colors,
           reduction_func = "first") %>%
    rasterly_points(color = fire, group_by_data_table = FALSE) %>%
    rasterly_build() -> ds
  expect_equal(is.rasterly(ds), TRUE)
  
  ########### "last" ##############
  rasterly(data,
           mapping = aes(x = x, y = y, on = x),
           background = "black",
           x_range = c(min_x, max_x),
           y_range = c(min_y, max_y),
           color = colors,
           reduction_func = "last") %>%
    rasterly_points(color = fire, group_by_data_table = FALSE) %>%
    rasterly_build() -> ds
  expect_equal(is.rasterly(ds), TRUE)
  
  # reduction function is any and group_by_data_table is FALSE
  rasterly(data,
           mapping = aes(x = x, y = y, on = x),
           background = "black",
           x_range = c(min_x, max_x),
           y_range = c(min_y, max_y),
           color = colors,
           reduction_func = "last") %>%
    rasterly_points(color = fire, group_by_data_table = FALSE) %>%
    rasterly_build() -> ds
  expect_equal(is.rasterly(ds), TRUE)
  
  ########### "m2" ##############
  rasterly(data,
           mapping = aes(x = x, y = y, on = x),
           background = "black",
           x_range = c(min_x, max_x),
           y_range = c(min_y, max_y),
           color = colors,
           reduction_func = "m2") %>%
    rasterly_points(color = fire, group_by_data_table = FALSE) %>%
    rasterly_build() -> ds
  expect_equal(is.rasterly(ds), TRUE)
  
  # reduction function is any and group_by_data_table is FALSE
  rasterly(data,
           mapping = aes(x = x, y = y, on = x),
           background = "black",
           x_range = c(min_x, max_x),
           y_range = c(min_y, max_y),
           color = colors,
           reduction_func = "m2") %>%
    rasterly_points(color = fire, group_by_data_table = FALSE) %>%
    rasterly_build() -> ds
  expect_equal(is.rasterly(ds), TRUE)
  
  ########### "max" ##############
  rasterly(data,
           mapping = aes(x = x, y = y, on = x),
           background = "black",
           x_range = c(min_x, max_x),
           y_range = c(min_y, max_y),
           color = colors,
           reduction_func = "max") %>%
    rasterly_points(color = fire, group_by_data_table = FALSE) %>%
    rasterly_build() -> ds
  expect_equal(is.rasterly(ds), TRUE)
  
  # reduction function is any and group_by_data_table is FALSE
  rasterly(data,
           mapping = aes(x = x, y = y, on = x),
           background = "black",
           x_range = c(min_x, max_x),
           y_range = c(min_y, max_y),
           color = colors,
           reduction_func = "max") %>%
    rasterly_points(color = fire, group_by_data_table = FALSE) %>%
    rasterly_build() -> ds
  expect_equal(is.rasterly(ds), TRUE)
  
  # add_rasterly_heatmap
  p <- plot_ly(data = data) %>%
    add_rasterly_heatmap(x = ~x, y = ~y)
  p
  expect_equal(inherits(p, "plotly"), TRUE)
  
  # ggRasterly
  g <- ggRasterly(data,
                  mapping = aes(x = x, y = y),
                  plot_width = 300, plot_height = 400,
                  x_range = c(min_x, max_x),
                  y_range = c(min_y, max_y))
  expect_equal(inherits(g, "gg"), TRUE)
  
  # plotRasterly as points
  p <- plotRasterly(data,
                    mapping = aes(x = x, y = y),
                    plot_width = 300, plot_height = 400,
                    x_range = c(min_x, max_x),
                    y_range = c(min_y, max_y),
                    as_image = TRUE)
  expect_equal(inherits(p, "plotly"), TRUE)
  
  # plotRasterly as image
  p <- plotRasterly(data,
                    mapping = aes(x = x, y = y),
                    plot_width = 300, plot_height = 400,
                    x_range = c(min_x, max_x),
                    y_range = c(min_y, max_y),
                    as_image = FALSE)
  expect_equal(inherits(p, "plotly"), TRUE)
})
