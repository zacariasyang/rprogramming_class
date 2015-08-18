makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # make new matrix obsoletes the old inversion
  set <- function(y = matrix()) {
    x <<- y # overwrite with new matrix from outside
    inv <<- NULL # new matrix obsoletes the old inversion from outside
  }
  get <- function() x # get the current matrix
  setinv <- function(inversion) inv <<- inversion # write in inversion from outside
  getinv <- function() inv # get the current inversion
  list(set = set, get = get, # return a set of functions
       setinv = setinv,
       getinv = getinv)
}