
funcmap = function() {

  nameurl <- "http://finzi.psych.upenn.edu/R/library/" 
  tmp1 <- url(nameurl,"r")
  tmp2 <- readLines(tmp1)
  close(tmp1)
  tmp3 <- tmp2[c(grep("DIR", as.character(tmp2), perl = TRUE))]
  tmp4 <- tmp3[2:length(tmp3)]

  rm(tmp2)
  rm(tmp3)
  
  names <- character(length(tmp4))
  for (i in 1:length(tmp4)) {

    tmp1 <- unlist(strsplit(tmp4[i], "/"))
    tmp2 <- unlist(strsplit(tmp1[5], '>'))
    names[i] <- tmp2[2]
    rm(tmp1)
    rm(tmp2)

  }#FOR
  
  len_pack <- length(names)
  name_fun <- vector(len_pack, mode = "list")
  len_nodes <- array(0, len_pack)
  
  for (i in 1:len_pack) {

    s2 <- names[i]
    s3 <- "/html/"
    s123 <- paste(nameurl, s2, s3, sep = "")
    tmp <- suppressWarnings(try(url(s123, open='rb'), silent = TRUE))

    cat("* downloading", names[i], ".\n")

    if(sum(grep("Error", tmp[1])) < 1) {

      close(tmp)
      rm(tmp)      
      uall <- url(s123,"r")
      ruall <- readLines(uall)
      close(uall)
      tmp1 <- ruall[c(grep("valign", as.character(ruall), perl = TRUE))]
      tmp2 <- tmp1[c(grep(".html", as.character(tmp1), perl = TRUE))] 
      rm(tmp1)
      len <- length(tmp2)
      tmp5 <- array(0,len) 
      if (len > 0) {

        for(j in 1:len) {

          tmp3 <- unlist(strsplit(tmp2[j],'.html'))
          tmp4 <- unlist(strsplit(tmp3[2],'>'))
          tmp5[j] <- tmp4[2]
          # rm(tmp3)
          # rm(tmp4)

        }#FOR

      }#THEN

      name_fun[[i]] <- tmp5[which(tmp5 != "00Index")]
      len_nodes[i] <- length(name_fun[[i]])
      rm(tmp2)
      rm(tmp5)

    }#THEN

  }#FOR

  adjmat <- matrix(0L, len_pack, len_pack, dimnames = list(names, names))

 
  for (i in 1:len_pack) {

    tmp1 <- name_fun[[i]]

    cat("------------>", names[i], "\n")
    print(name_fun[[i]])

    for (j in min(i+1, len_pack-1):len_pack) {

        tmp2 <- name_fun[[j]]
        tmp3 <- my.intersect(tmp1, tmp2)

         if (length(tmp3) > 0) adjmat[i, j] <- 1L

        # rm(tmp2)
        # rm(tmp3)

    }#FOR

    # rm(tmp1)

  }#FOR

  return(adjmat)

}#FUNCMAP

my.intersect = function (x, y) {

  y[match(x, y, 0L)]

}#MY.INTERSECT
