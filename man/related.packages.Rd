\name{related.packages}
\alias{related.packages}
\title{ Plot a package's neighbourhood }
\description{

  Plot a graph containing the set of packages closely related
  by forward or reverse dependencies to a specified package.

}
\usage{
related.packages(x, node = NULL, order = 1, graphviz = FALSE)
}
\arguments{
  \item{x}{an object of class \code{igraph} returned by \code{map.depends}.}
  \item{node}{a character string, the name of a package present in \code{x}.}
  \item{order}{a positive integer, giving the order of the neighbourhood 
    (i.e. the maximum distance from the package specified by the \code{node}
    parameter).}
  \item{graphviz}{a boolean value. If \code{TRUE} \pkg{Rgraphviz} is loaded
    and used to produce the plot; if \code{FALSE} \pkg{igraph} plotting 
    functions are used instead.}
}
\value{

  The object of class \code{igraph} used for the plot.

}
\examples{
\dontrun{
d = map.depends()
related.packages(d, 1, "boot")
}
}
\author{ Marco Scutari }
\keyword{graphs}
\keyword{hplot}
