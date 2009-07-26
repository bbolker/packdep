\name{packdep-package}
\alias{packdep-package}
\alias{packdep}
\docType{package}
\title{

  Mapping dependencies among R packages

}
\description{

  Mapping dependencies among R packages, both on CRAN and BioConductor.

}
\details{

  \tabular{ll}{

    Package: \tab packdep\cr
    Type: \tab Package\cr
    Version: \tab 0.2\cr
    Date: \tab 2009-07-26\cr
    License: \tab GPLv2 or later

  }


  Reusing user-contributed packages is encouraged in R open-source environment 
  and promotes transparency, reproducibility  while minimizing redundancy. 
  However, package inter-dependencies can have a significant impact on their 
  sustainability. 

  User-contributed R-packages undergo revisions with time and may even be 
  orphaned in the absence of an active maintainer. \pkg{packdep} elucidates 
  the dependencies (\dQuote{Suggests}, \dQuote{Imports} and \dQuote{Depends})
  between user contributed R packages in CRAN and BioConductor. It subsequently
  identifies key packages by ranking them using a battery of social network 
  analysis metrics. With the number of user-contributed packages increasing
  steadily with time, understanding dependencies between R packages may be 
  critical for sustainable software development. \pkg{packdep} may also provide
  preliminary insights into identifying \dQuote{recommended} packages.

}
\author{

  Radhakrishnan Nagarajan \email{rnagarajan@uams.edu}\cr
  Department of Biostatistics\cr
  University of Arkansas for Medical Sciences

  Marco Scutari \email{marco.scutari@stat.unipd.it}\cr
  Department of Statistical Sciences\cr
  University of Padova

}
\references{

  Wasserman S, Faust K (2007) \emph{Social Network Analysis}. Cambridge 
      University Press. 

}
\examples{
library(packdep)

\dontrun{
# map dependencies among CRAN packages.
d1 = map.depends()
# use a specific CRAN mirror.
d2 = map.depends(contriburl = contrib.url("http://cran.r-project.org"))
d2
# map dependencies among BioConductor packages.
d3 = map.depends(repository = "bioc")
d3
}
}
\keyword{ package }
