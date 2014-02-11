
# plot a map of the neighbourhood of a package.
related.packages = function(x, node = NULL, order = 1, graphviz = FALSE) {

  if (!is(x, "igraph"))
    stop("'x' must be an object of class 'igraph'.")

  if (!(is.numeric(order) && (length(order) == 1) && is.finite(order) && 
       (order > 0) && ((order %/% 1) == order)))
    stop("order must be a positive integer.")

  if (is.character(node) && length(node) == 1) {

    if (node %in% V(x)$name)
      target = which(V(x)$name %in% node) - 1
    else
      stop(paste("unknown package ", target, ".", sep = ""))

  }#THEN
  else {

    stop("'node' must be a character string, the name of a package.")

  }#ELSE

  # create the desired subgraph.
  n = neighborhood(x, order, target)[[1]]
  s = subgraph(x, n)

  if (graphviz == FALSE) {

    # set the color of the nodes.
    V(s)$color = "SkyBlue2"
    V(s)[V(s)$name == node]$color = "red"

    # plot.
    plot(s, vertex.label = V(s)$name, layout = layout.kamada.kawai(s))

  }#THEN
  else {

    if(!require('Rgraphviz'))
      stop("Rgraphviz cannot be loaded.")

    graph.obj = igraph.to.graphNEL(s)
    
    attrs = list(node = list(fixedsize = FALSE))
    node.attrs = list(shape = rep("ellipse", length(V(s)$name)))
    names(node.attrs$shape) = V(s)$name

    graph.plot = layoutGraph(graph.obj, nodeAttrs = node.attrs)

    renderGraph(graph.plot)

  }#ELSE

  # return the igraph object for further manipulations.
  return(s)

}#RELATED.PACKAGES

