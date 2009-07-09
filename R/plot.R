
# histogram method for centrality.
hist.packdep.centrality = function(x, type = c("betweenness", "closeness"), 
    xlab = NULL, ylab = "frequency", main = "", ...) {

  type = match.arg(type)

  histogram.backend(x, type = type, xlab = xlab, ylab = ylab, 
    main = main, ...)

}#HIST.PACKDEP.CENTRALITY

# histogram method for dependencies.
hist.packdep.dependencies = function(x, type = c("dependencies", "reverse"), 
    xlab = NULL, ylab = "frequency", main = "", ...) {

  type = match.arg(type)

  histogram.backend(x, type = type, xlab = xlab, ylab = ylab, 
    main = main, ...)

}#HIST.PACKDEP.DEPENDENCIES

# plot method for centrality.
plot.packdep.centrality = function(x, type = c("betweenness", "closeness"), 
    logscale = c("", "x", "y", "xy"), breaks = 10, freq = FALSE, 
    xlab = NULL, ylab = NULL, ...) {

  type = match.arg(type)
  logscale = match.arg(logscale)

  base.plot.backend(x, type = type, logscale = logscale, breaks = breaks,
    freq = freq, xlab = xlab, ylab = ylab, ...)

}#PLOT.PACKDEP.CENTRALITY

# plot method for dependencies.
plot.packdep.dependencies = function(x, type = c("dependencies", "reverse"), 
    logscale = c("", "x", "y", "xy"), breaks = 10, freq = FALSE,
    xlab = NULL, ylab = NULL, ...) {

  type = match.arg(type)
  logscale = match.arg(logscale)

  base.plot.backend(x, type = type, logscale = logscale, breaks = breaks,
    freq = freq, xlab = xlab, ylab = ylab, ...)

}#PLOT.PACKDEP.DEPENDENCIES

# backend for the hist() methods.
histogram.backend = function(x, type, xlab, ylab, main, ...) {

  if (is.null(xlab))
    xlab = type

  hist(x[, type], xlab = xlab, ylab = ylab, main = main, ...)

}#HISTOGRAM.BACKEND

# backend for the plot() methods.
base.plot.backend = function(x, type, logscale, breaks, freq, xlab, 
    ylab, ...) {

  # compute the observed frequencies.
  h = hist(x[, type], breaks = breaks, plot = FALSE)

  # adjust the axes' labels for logscale.
  if (is.null(xlab))
    xlab = ifelse (logscale %in% c("x", "xy"), paste(type, "(log scale)"), type)
  if (is.null(ylab)) 
    ylab = ifelse (logscale %in% c("y", "xy"), "frequency (log scale)", "frequency")

  # set all zero frequencies to 1, so that they get back to zero
  # when plotted on a logarithmic scale.
  if (logscale %in% c("y", "xy")) {

    h$counts[h$counts == 0] = .5
    h$density[h$density == 0] = min(h$density[h$density > 0])/2

  }#THEN

  if (freq)
    plot(h$mids, h$density, type = "b", log = logscale, xlab = xlab, 
      ylab = ylab, ...)
  else
    plot(h$mids, h$counts, type = "b", log = logscale, xlab = xlab, 
      ylab = ylab, ...)

}#BASE.PLOT.BACKEND

