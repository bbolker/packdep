\name{plot centrality}
\alias{plot centrality}
\alias{plot.packdep.centrality}
\alias{hist.packdep.centrality}
\title{ Plot closeness and betweenness. }
\description{

  Plot closeness and betweenness from the
  object returned by \code{centrality}.

}
\usage{
  \method{plot}{packdep.centrality}(x, type = c("betweenness", "closeness"), 
    logscale = c("", "x", "y", "xy"), breaks = 10, freq = FALSE,
    xlab = NULL, ylab = NULL, ...)
  \method{hist}{packdep.centrality}(x, type = c("betweenness", "closeness"), 
    xlab = NULL, ylab = "frequency", main = "", ...)
}
\arguments{
  \item{x}{an object of class \code{packdep.centrality}.}
  \item{type}{a character string. Possible choices are \code{closeness}
    (closeness values are plotted) and \code{betweenness} (betweenness
    values are plotted).}
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
\seealso{\code{\link{centrality}}.}
\author{ Radhakrishnan Nagarajan and Marco Scutari }
\examples{
\dontrun{
d = get.depends()
c = centrality(d)

hist(c, freq = TRUE)
plot(c, freq = FALSE, logscale = "y")
}
}
\keyword{hplot}
