
# store packages' dependencies in an adjacency matrix.
map.depends = function(repository = c("cran", "bioc"), contriburl = contrib.url(getOption("repos"))) {

  # choose one: CRAN or BioConductor.
  repository = match.arg(repository)

  if (repository == "cran") {

    # retrieve packages' information from CRAN.
    a = available.packages(contriburl = contriburl)

  }#THEN
  else if (repository == "bioc") {

    # install the BioConductor repository.
    source("http://bioconductor.org/biocLite.R")

    # retrieve packages' information from BioConductor.
    a = available.packages(contriburl = contrib.url(grep(pattern = "bioconductor", x = biocinstallRepos(), value = TRUE)))

  }#THEN

  # extract Depends, Import and Suggests for each package.
  imp = tools:::package.dependencies(a, depLevel = "Imports")
  sug = tools:::package.dependencies(a, depLevel = "Suggests")
  dep = tools:::package.dependencies(a, depLevel = "Depends")

  # store the names of the packages.
  name_pack = rownames(a)

  if (repository == "cran") {

    # retrieve the information of base and recommended packages, which are not
    # present on CRAN (they are not "contributed", but part of the R core).
    inst = installed.packages()
    inst = rownames(inst[inst[, "Priority"] %in% c("base", "recommended"), , drop = FALSE])
    # combine the two sets of packages.
    name_pack = union(inst, name_pack)

  }#THEN

  # allocate and initialize the adjacency matrix.
  m = matrix(0L, nrow = length(name_pack),
        ncol = length(name_pack),
        dimnames = list(name_pack, name_pack))

  # fill in the information about each package.
  for (i in name_pack) {

    if (is.matrix(dep[[i]])) {
      valid = name_pack %in% dep[[i]][, 1, drop = TRUE]
      m[ valid , i] <- 1L
    }#THEN

    if (is.matrix(sug[[i]])) {
      valid = name_pack %in% sug[[i]][, 1, drop = TRUE]
      m[ valid , i] <- 1L
    }#THEN

    if (is.matrix(imp[[i]])) {
      valid = name_pack %in% imp[[i]][, 1, drop = TRUE]
      m[ valid , i] <- 1L
    }#THEN

  }#FOR

  return(graph.adjacency(m, mode = c("directed")))

}#GET.DEPENDS

