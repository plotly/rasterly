################################ Unreported functions in `ggplot2` and `plotly`
# Reason: Avoid `:::` to pass R CMD check

## Unexported functions in ggplot2
new_aes <- getFromNamespace("new_aes", "ggplot2")

new_aesthetic <- getFromNamespace("new_aesthetic", "ggplot2")

## Unexported functions in plotly
add_trace_classed <- function(p, class = "plotly_polygon", ...) {
  p <- plotly::add_trace(p, ...)
  nAttrs <- length(p$x$attrs)
  p$x$attrs[[nAttrs]] <- prefix_class(p$x$attrs[[nAttrs]], class)
  p
}

prefix_class <- function(x, y) {
  structure(x, class = unique(c(y, class(x))))
}
