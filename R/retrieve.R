
# store packages' dependencies in an adjacency matrix.
map.depends = function(repository = c("cran", "bioc"), contriburl = contrib.url(getOption("repos")), 
    dependencies = c("Imports", "Depends", "Suggests")) {

  # choose one: CRAN or BioConductor.
  if (!is.character(repository) || any(!(repository %in% c("cran", "bioc"))))
    stop("repository must be either 'cran' (for CRAN) or 'bioc' (for BioConductor).")
  repository = match.arg(repository)
  if (length(repository) != 1)
    stop("only one repository can be specified.")
  # choose one or more form of dependency.
  if (!is.character(dependencies) || any(!(dependencies %in% c("Imports", "Depends", "Suggests"))))
    stop("dependencies must be either 'Imports', 'Depends' or 'Suggests'.")

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

  # check whether the connection to the mirror was successful.
  if (nrow(a) == 0)
    stop(paste("unable to fetch dependency data from", contriburl, "."))

  # extract Depends, Import and Suggests for each package.
  if ("Imports" %in% dependencies)
    imp = package.dependencies(a, depLevel = "Imports")
  if ("Suggests" %in% dependencies)
    sug = package.dependencies(a, depLevel = "Suggests")
  if ("Depends" %in% dependencies)
    dep = package.dependencies(a, depLevel = "Depends")

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

    if ("Depends" %in% dependencies) {

      if (is.matrix(dep[[i]])) {
        valid = name_pack %in% dep[[i]][, 1, drop = TRUE]
        m[ valid , i] <- 1L
      }#THEN

    }#THEN

    if ("Suggests" %in% dependencies) {

      if (is.matrix(sug[[i]])) {
        valid = name_pack %in% sug[[i]][, 1, drop = TRUE]
        m[ valid , i] <- 1L
      }#THEN

    }#THEN

    if ("Imports" %in% dependencies) {

      if (is.matrix(imp[[i]])) {
        valid = name_pack %in% imp[[i]][, 1, drop = TRUE]
        m[ valid , i] <- 1L
      }#THEN

    }#THEN

  }#FOR

  return(graph.adjacency(m, mode = c("directed")))

}#GET.DEPENDS

