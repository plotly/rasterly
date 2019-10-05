################################ Unreported functions in `ggplot2` and `plotly`
# Reason: Avoid `:::` to pass R CMD check

## Unexported function in ggplot2
new_aes <- function(x, env = globalenv()) {
  stopifnot(is.list(x))
  x <- lapply(x, new_aesthetic, env = env)
  structure(x, class = "uneval")
}

new_aesthetic <- function(x, env = globalenv()) {
  if (rlang::is_quosure(x)) {
    if (!rlang::quo_is_symbolic(x)) {
      x <- rlang::quo_get_expr(x)
    }
    return(x)
  }
  
  if (rlang::is_symbolic(x)) {
    x <- rlang::new_quosure(x, env = env)
    return(x)
  }
  
  x
}

## Unexported function in plotly
add_trace_classed <- function(p, class = "plotly_polygon", ...) {
  p <- plotly::add_trace(p, ...)
  nAttrs <- length(p$x$attrs)
  p$x$attrs[[nAttrs]] <- prefix_class(p$x$attrs[[nAttrs]], class)
  p
}

prefix_class <- function(x, y) {
  structure(x, class = unique(c(y, class(x))))
}