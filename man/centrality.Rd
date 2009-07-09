\name{centrality}
\alias{centrality}
\title{ Packages' betweenness and closeness }
\description{

  Estimate packages' betweenness and closeness indexes.

}
\usage{
  centrality(x, order.by = c("none", "betweenness", "closeness"))
}
\arguments{
  \item{x}{an object of class \code{igraph} returned by \code{get.depends}.}
  \item{order.by}{a character string. Possible values are \code{none}
    (rows in the returned data frame are not sorted), \code{betweenness}
    (rows are sorted in order of decreasing betweenness) or \code{closeness}
    (rows are sorted in order of decreasing closeness).}
}
\value{

  A data frame with the following columns:

  \item{package}{the name of the package.}
  \item{betweenness}{the betweenness centrality measure for the package.}
  \item{closeness}{the closeness centrality measure for the package.}

  The class of the data frame is changed to \code{c("packdep.centrality",
  "data.frame")} to override the dispatch of the plotting methods
  (currently \code{plot} and \code{hist}).

}
\seealso{\code{\link{map.depends}} and \code{\link{plot centrality}}.}
\references{

  Wasserman S, Faust K (2007) \emph{Social Network Analysis}. Cambridge
      University Press.

}
\examples{
\dontrun{
d = get.depends()
centrality(d)

# write a table containing the packages on CRAN,
# ordered by decreasing betweenness, to a specified file.
res = centrality(d, order.by = "betweenness")
write.table(res, file = "centrality.txt", row.names = FALSE)
}
}
\author{ Radhakrishnan Nagarajan and Marco Scutari }
\keyword{graphs}
