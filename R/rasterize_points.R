#' @title rasterize_points
#' @description Points layer for "rasterly". Deprecated now, please use \code{rasterly_points} instead.
#' @seealso \link{rasterly_points}
#' @inheritParams rasterly_points
#' @export
rasterize_points <- function(rastObj,
                             data = NULL,
                             mapping = aes(),
                             ...,
                             xlim = NULL,
                             ylim = NULL,
                             max_size = NULL,
                             reduction_func = NULL,
                             layout = NULL,
                             glyph = NULL,
                             group_by_data_table = NULL,
                             inherit.aes = TRUE) {
  warning("`rasterize_points` is deprecated. Please use `rasterly_points`.")
  rasterly_points(
    rastObj,
    data,
    mapping,
    ...,
    xlim,
    ylim,
    max_size,
    reduction_func,
    layout,
    glyph,
    group_by_data_table,
    inherit.aes
  )
}