#' @title Merge operator
#' @description Merge two objects from right to left
#' @usage x \%<-\% y
#' 
#' @return a list
#' 
#' @param x A named list or vector
#' @param y A named list or vector. Any duplicated names are detected in x will be covered by y
#' 
#' @examples 
#' # two lists
#' x <- list(a = 1, b = "foo", c = 3)
#' y <- list(b = 2, d = 4)
#' x %<-% y
#' y %<-% x
#' 
#' # one list and one vector
#' x <- c(foo = 1, bar = 2)
#' y <- list(foo = "foo")
#' x %<-% y
#' y %<-% x
#' 
#' # two vectors
#' x <- c(a = 1, b = "foo", c = 3)
#' y <- c(b = 2, d = 4)
#' x %<-% y
#' y %<-% x
#' 
#' # duplicated names in x
#' x <- list(a = 1, b = "foo", b = 3)
#' y <- list(b = 2, d = 4)
#' x %<-% y
#' y %<-% x # be careful, since 3 will cover on "foo" in x, then on 2 in y
#' 
#' @export
`%<-%` <- function(x, y) {
  if(is.null(names(x)) || is.null(names(y))) 
    return(c(x,y))
  else {
    
    if(!is.list(x)) x <- as.list(x)
    if(!is.list(y)) y <- as.list(y)
    
    merged_list <- c(x, y)
    list_names <- names(merged_list)
    merged_list[duplicated(list_names, fromLast = TRUE)] <- NULL
    
    return(merged_list[unique(list_names)])
  }
}
