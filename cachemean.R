cachemean <- function(x, ...) {
  m <- x$getmean() # fetch the mean through pre-defined interface
  if(!is.null(m)) { # figure out if there is any mean cached
    message("getting cached data")
    return(m)
  }
  data <- x$get() # in the case of no mean previously cached, start to calculate the mean
  m <- mean(data, ...) # ... carries additional arguments of mean fucntion
  x$setmean(m) # write in the mean
  m
}