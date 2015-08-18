makeVector <- function(x = numeric()) {
  m <- NULL # make new vector obsoletes the old mean
  set <- function(y) {
    x <<- y # overwrite with new vector from outside
    m <<- NULL # new vector obsoletes the old mean from outside
  }
  get <- function() x # get the current vector
  setmean <- function(mean) m <<- mean # write in mean from outside
  getmean <- function() m # get the current mean
  list(set = set, get = get, # return a set of functions
       setmean = setmean,
       getmean = getmean)
}