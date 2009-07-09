\name{related.packages}
\alias{related.packages}
\title{ Plot a package's neighbourhood }
\description{

  Plot a graph containing the set of packages closely related
  by forward or reverse dependencies to a specified package.

}
\usage{
  related.packages(x, order = 1, node)
}
\arguments{
  \item{x}{an object of class \code{igraph} returned by \code{get.depends}.}
  \item{order}{a positive integer, giving the order of the neighbourhood 
    (i.e. the maximum distance from the package specified by the \code{node}
    parameter).}
  \item{node}{a character string, the name of a package present in \code{x}.}
}
\value{

  The object of class \code{igraph} used for the plot.

}
\examples{
\dontrun{
d = get.depends()
related.packages(d, 1, "boot")
}
}
\author{ Marco Scutari }
\keyword{graphs}
\keyword{hplot}
