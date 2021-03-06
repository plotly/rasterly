% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/merge_operator.R
\name{\%<-\%}
\alias{\%<-\%}
\title{Merge operator}
\usage{
x \%<-\% y
}
\arguments{
\item{x}{A named list or vector}

\item{y}{A named list or vector. Any duplicated names are detected in x will be covered by y}
}
\value{
a list
}
\description{
Merge two objects from right to left.
}
\examples{
# two lists
x <- list(a = 1, b = "foo", c = 3)
y <- list(b = 2, d = 4)
x \%<-\% y
y \%<-\% x

# one list and one vector
x <- c(foo = 1, bar = 2)
y <- list(foo = "foo")
x \%<-\% y
y \%<-\% x

# two vectors
x <- c(a = 1, b = "foo", c = 3)
y <- c(b = 2, d = 4)
x \%<-\% y
y \%<-\% x

# duplicated names in x
x <- list(a = 1, b = "foo", b = 3)
y <- list(b = 2, d = 4)
x \%<-\% y
y \%<-\% x # be careful, since "3" will cover on "foo" in x, then on "2" in y

}
