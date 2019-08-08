count <- function(input = 0,
                  on = NULL,
                  index = integer(0)) {

  if(is.null(on) || length(index) == 0) input + 1 else input + on[index]
}
