\name{map.depends}
\alias{map.depends}
\title{ Retrieve packages' dependencies }
\description{

  Retrieve packages' dependencies from either a CRAN mirror
  or BioConductor.

}
\usage{
map.depends(repository = c("cran", "bioc"), 
  contriburl = contrib.url(getOption("repos")),
  dependencies = c("Imports", "Depends", "Suggests")) 
}
\arguments{
  \item{repository}{a character string, either \code{cran} (for a CRAN mirror)
    or \code{bioc} (for BioConductor).}
  \item{contriburl}{URL(s) of the contrib sections of the repositories.}
  \item{dependencies}{a character vector, specifying which kind of dependencies
    are to be evaluated. Possible values are one or more among \dQuote{Imports},
    \dQuote{Depends} and \dQuote{Suggests}.}
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

  An object of class \code{igraph}, representing the dependencies
  as an directed acyciclic graph.

}
\author{ Radhakrishnan Nagarajan and Marco Scutari }
\keyword{graphs}
