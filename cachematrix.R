## Below are two functions that are used to create a special object that
## stores a matrix and cache's its inverse.
## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inver <<- inverse
  getInverse <- function() inver
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
## The following function calculates the inverse of the special "matrix" created
## with the above function. However, it first checks to see if the inverse has
## already been calculated. If so, it gets the inverse from the cache and
## skips the computation. Otherwise, it calculates the inverse of the data
## and sets the value of the inverse in the cache via the setmean function.

cacheSolve <- function(x, ...) {
  
  inver <- x$getInverse()
  if (!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  mat <- x$get()
  inver <- solve(mat, ...)
  x$setInverse(inver)
  inver
}
