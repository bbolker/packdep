\name{funcmap}
\alias{funcmap}
\title{ Map similar funtionality in CRAN's packages }
\description{

  Find out which packages share similar functions or topics.

}
\usage{
funcmap()
}
\details{

  This functions retrieves help pages' titles from all the packages
  on CRAN and judges whether the latter are related by intersection
  (i.e. if the same title appears in two packages they are considered
  related).

}
\value{

  An adjacency matrix; elements with a value of \code{1} denote
  packages for which at least one match have been found.

}
\examples{
\dontrun{
z = funcmap()
z["bnlearn", "deal"]
z["bnlearn", "ggm"]
z["bnlearn", "igraph"]
}
}
\author{ Radhakrishnan Nagarajan }
\keyword{graphs}
