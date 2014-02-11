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
    Version: \tab 0.3.1\cr
    Date: \tab 2014-02-11\cr
    License: \tab GPLv2 or later

  }

  Re-using user-contributed packages is encouraged in R open-source environment
  and promotes transparency, reproducibility while minimizing redundancy. However,
  understanding package inter-dependencies can significantly improve sustainability. 

  User-contributed R packages undergo revisions with time, while some of the packages
  are orphaned, others are re-used extensively as (\dQuote{Suggests}, \dQuote{Imports}
  and \dQuote{Depends}). \pkg{packdep} elucidates the dependencies  between user 
  contributed R packages in CRAN and BioConductor. It subsequently identifies key 
  packages by ranking them using a battery of social network analysis metrics. 
  \pkg{packdep} can also be used to identify function names that are shared across 
  packages. This is especially important, since a given function name across two 
  different packages can have distinct purposes. With the number of user-contributed
  packages increasing steadily with time, understanding the dependencies between R
  packages and minimizing the reuse of existing function names may be critical for 
  sustainable software development. It is believed \pkg{packdep} may also provide 
  preliminary insights into identifying recommended packages and eventual resource
  allocation in the R open-source environment.

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
