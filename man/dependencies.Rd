\name{dependencies}
\alias{dependencies}
\title{ Count dependencies and reverse dependencies }
\description{

  Count dependencies and reverse dependencies.

}
\usage{
  dependencies(x, order.by = c("none", "dependencies", "reverse"))
}
\arguments{
  \item{x}{an object of class \code{igraph} returned by \code{map.depends}.}
  \item{order.by}{a character string. Possible values are \code{none}
    (rows in the returned data frame are not sorted), \code{dependencies}
    (rows are sorted in order of decreasing number of dependencies) or \code{reverse}
    (rows are sorted in order of decreasing number of reverse dependencies).}
}
\details{

  This function uses \code{available.packages} to get the
  Depends, Imports and Suggests for all packages in a CRAN
  or BioConductor mirror.

  If \code{repository} is set to \code{bioc}, the \code{contriburl}
  parameter is ignored and the URL of BioConductor is set via the
  \code{biocinstallRepos} function.

}
\value{

  A data frame with the following columns:

  \item{package}{the name of the package.}
  \item{dependencies}{number dependencies of the package.}
  \item{reverse}{number of reverse dependencies (i.e. the number
    of packages that have the package in question as a dependency)
    of the package.}

  The class of the data frame is changed to \code{c("packdep.dependencies",
  "data.frame")} to override the dispatch of the plotting methods
  (currently \code{plot} and \code{hist}).

}
\seealso{\code{\link{map.depends}} and \code{\link{plot dependencies}}.}
\examples{
\dontrun{
d = map.depends()
dependencies(d)

# write a table containing the packages on CRAN,
# ordered by decreasing betweenness, to a specified file.
res = dependencies(d, order.by = "reverse")
write.table(res, file = "reverse.dependencies.txt", row.names = FALSE)
}
}
\author{ Radhakrishnan Nagarajan and Marco Scutari }
\keyword{graphs}
