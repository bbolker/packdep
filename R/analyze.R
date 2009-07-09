
# dependencies and reverse dependecies.
dependencies = function(x, order.by = c("none", "dependencies", "reverse")) {

  if (!is(x, "igraph"))
    stop("'x' must be an object of class 'igraph'.")

  order.by = match.arg(order.by)

  res = data.frame(package = V(x)$name, 
                   dependencies = degree(x, mode = "in"), 
                   reverse = degree(x, mode = "out"),
                   row.names = V(x)$name,
                   stringsAsFactors = FALSE)

  if (order.by != "none")
    res = res[order(res[, order.by], decreasing = TRUE), , drop = FALSE]

  class(res) = c("packdep.dependencies", class(res))

  return(res)

}#DEPENDENCIES

# measures of centrality
centrality = function(x, order.by = c("none", "betweenness", "closeness")) {

  if (!is(x, "igraph"))
    stop("'x' must be an object of class 'igraph'.")

  order.by = match.arg(order.by)

  res = data.frame(package = V(x)$name, 
                   betweenness = betweenness(x, directed = TRUE), 
                   closeness = closeness(x, mode = c("out")),
                   row.names = V(x)$name,
                   stringsAsFactors = FALSE)

  if (order.by != "none")
    res = res[order(res[, order.by], decreasing = TRUE), , drop = FALSE]

  class(res) = c("packdep.centrality", class(res))

  return(res)

}#CENTRALITY

