
# dependencies and reverse dependecies.
dependencies = function(x, order.by = c("none", "dependencies", "reverse"),
    decreasing = TRUE) {

  if (!is(x, "igraph"))
    stop("'x' must be an object of class 'igraph'.")

  # choose the order of the pacakges.
  if (!is.character(order.by) || any(!(order.by %in% c("none", "dependencies", "reverse"))))
    stop("dependencies may be unordered ('none') or be ordered either by 'dependencies' or by 'reverse'.")
  order.by = match.arg(order.by)
  if (length(order.by) != 1)
    stop("only one ordering can be specified.")
  # check decreasing.
  if (!is.logical(decreasing) || is.na(decreasing) || (length(decreasing) != 1))
    stop("decreasing must be a logical value (TRUE/FALSE).")

  labels = V(x)$name

  res = data.frame(package = labels, 
                   dependencies = degree(x, mode = "in"), 
                   reverse = degree(x, mode = "out"),
                   row.names = labels,
                   stringsAsFactors = FALSE)

  if (order.by != "none")
    res = res[order(res[, order.by], decreasing = decreasing), , drop = FALSE]

  class(res) = c("packdep.dependencies", class(res))

  return(res)

}#DEPENDENCIES

# measures of centrality.
centrality = function(x, order.by = c("none", "betweenness", "incloseness", 
    "outcloseness", "indegree", "outdegree", "degree"), decreasing = TRUE) {

  if (!is(x, "igraph"))
    stop("'x' must be an object of class 'igraph'.")

  # choose the order of the pacakges.
  if (!is.character(order.by) || 
      any(!(order.by %in% c("none", "betweenness", "incloseness", "outcloseness", 
                            "indegree", "outdegree", "degree"))))
    stop("dependencies may be unordered ('none') or be ordered either by 'dependencies' or by 'reverse'.")
  order.by = match.arg(order.by)
  if (length(order.by) != 1)
    stop("only one ordering can be specified.")
  # check decreasing.
  if (!is.logical(decreasing) || is.na(decreasing) || (length(decreasing) != 1))
    stop("decreasing must be a logical value (TRUE/FALSE).")

  vertices = V(x)
  labels = vertices$name

  res = data.frame(package = labels, 
                   betweenness = betweenness(x, v = vertices, directed = TRUE), 
                   incloseness = closeness(x, vids = vertices, mode = c("in")),
                   outcloseness = closeness(x, vids = vertices, mode = c("out")),
                   indegree = degree(x, v = vertices, mode = c("in")),
                   outdegree = degree(x, v = vertices, mode = c("out")),
                   degree = degree(x, v = vertices, mode = c("total")),
                   row.names = labels,
                   stringsAsFactors = FALSE)

  if (order.by != "none")
    res = res[order(res[, order.by], decreasing = decreasing), , drop = FALSE]

  class(res) = c("packdep.centrality", class(res))

  return(res)

}#CENTRALITY

