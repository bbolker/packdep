\name{plot dependencies}
\alias{plot dependencies}
\alias{plot.packdep.dependencies}
\alias{hist.packdep.dependencies}
\title{ Plot dependencies and reverse dependencies. }
\description{

  Plot dependencies and reverse dependencies from the
  object returned by \code{dependencies}.

}
\usage{
  \method{plot}{packdep.dependencies}(x, type = c("dependencies", "reverse"), 
    logscale = c("", "x", "y", "xy"), breaks = 10, freq = FALSE, 
    xlab = NULL, ylab = NULL, ...)
  \method{hist}{packdep.dependencies}(x, type = c("dependencies", "reverse"), 
    xlab = NULL, ylab = "frequency", main = "", ...)
}
\arguments{
  \item{x}{an object of class \code{packdep.dependencies}.}
  \item{type}{a character string. Possible choices are \code{dependencies}
    (dependencies are plotted) and \code{reverse} (reverse dependencies
    are plotted).}
  \item{logscale}{a character string, which indicates which axes are to
    to be plotted on a logarithmic scale (none by default).}
  \item{breaks}{the breaks used in the plot; see \code{\link{hist}} 
    for details.}
  \item{freq}{a logical value. If \code{TRUE} absolute frequencies
    (counts) are used; if \code{FALSE} relative frequencies are used 
    instead.}
  \item{xlab}{a title for the x axis.}
  \item{ylab}{a title for the y axis.}
  \item{main}{an overall title for the plot.}
  \item{\dots}{other parameters to be passed through to plotting
    functions.}
}
\seealso{\code{\link{dependencies}}.}
\author{ Radhakrishnan Nagarajan and Marco Scutari }
\examples{
\dontrun{
d = get.depends()
c = dependencies(d)

hist(c, freq = TRUE)
plot(c, freq = FALSE, logscale = "y")
}
}
\keyword{hplot}
