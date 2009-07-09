
# plot a map of the neighbourhood of a package.
related.packages = function(x, order = 1, node) {

  if (!is(x, "igraph"))
    stop("'x' must be an object of class 'igraph'.")

  if (is.character(node) && length(node) == 1) {

    if (node %in% V(x)$name)
      target = which(V(x)$name %in% node) - 1
    else
      stop(paste("unknown package ", target, ".", sep = ""))

  }#THEN
  else {

    stop("'node' must be a character string , the name of a package.")

  }#ELSE

  # create the desired subgraph.
  n = neighborhood(x, order, target)[[1]]
  s = subgraph(x, n)

  # set the color of the nodes.
  V(s)$color = "SkyBlue2"
  V(s)[V(s)$name == node]$color = "red"

  # plot.
  plot(s, vertex.label = V(s)$name, layout = layout.kamada.kawai(s))

  # return the igraph object for further manipulations.
  return(s)

}

