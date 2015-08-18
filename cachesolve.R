cacheSolve <- function(x, ...) {
  inv <- x$getinv() # fetch the inversion through pre-defined interface
  if(!is.null(inv)) { # figure out if there is any inversion cached
    message("getting cached data!!!!!")
    return(inv)
  }
  data <- x$get() # in the case of no inversion previously cached, start to calculate the inversion
  inv <- solve(data, ...) # ... carries additional arguments of solve fucntion
  x$setinv(inv) # write in the inversion
  inv
}