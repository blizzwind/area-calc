library("plot.matrix")
library("dplyr")

chk <- function(i,j) {
  if (new_arr[i-1,j] == new_arr[i,j] & tmp[i-1,j] != 1) {
    tmp[i-1,j] <<- 1
    chk(i-1,j)
  }
  if (new_arr[i+1,j] == new_arr[i,j] & tmp[i+1,j] != 1) {
    tmp[i+1,j] <<- 1
    chk(i+1,j)
  }
  if (new_arr[i,j-1] == new_arr[i,j] & tmp[i,j-1] != 1) {
    tmp[i,j-1] <<- 1
    chk(i,j-1)
  }
  if (new_arr[i,j+1] == new_arr[i,j] & tmp[i,j+1] != 1) {
    tmp[i,j+1] <<- 1
    chk(i,j+1)
  }
}

tmp_grid <- function(i,j) {
  tmp <<- matrix(FALSE, nrow=dim(new_arr)[1], ncol=dim(new_arr)[2])
  tmp[i,j] <<- 1
  chk(i,j)
}

area_calc <- function(arr) {
  dat <- data.frame()
  new_arr <<- matrix(FALSE, nrow=dim(arr)[1]+2, ncol=dim(arr)[2]+2)
  for (i in 1:dim(arr)[1]) {
    for (j in 1:dim(arr)[2]) {
      new_arr[i+1,j+1] <<- arr[i,j]
    }
  }
  for (i in 2:(dim(new_arr)[1]-1)) {
    for (j in 2:(dim(new_arr)[2]-1)) {
      tmp_grid(i,j)
      count <- 0
      for (m in 1:dim(tmp)[1]) {
        for (n in 1:dim(tmp)[2]) {
          if (tmp[m,n] == 1) {
            count <- count+1
          }
        }
      }
      dat <- rbind(dat,c(new_arr[i,j],count))
    }
  }
  group <- c()
  for (i in 1:dim(dat)[1]) {
    times <- 0
    ref <- c(dat[i,1],dat[i,2])
    for (j in 1:dim(dat)[1]) {
      if (ref[1] == dat[j,1] & ref[2] == dat[j,2]) {
        times <- times+1
      }
    }
    group <- c(group,times/dat[i,2])
  }
  dat <- cbind(dat,group)
  dat <- distinct(dat)
  colnames(dat) <- c("VAL","COUNT","GROUP")
  plot(arr)
  return(dat)
}
