## Put comments here that give an overall description of what your
## functions do

## set/get matrix x and set/get matrix inverse 

makeCacheMatrix <- function(x = matrix()) {
  cached_inverse <- NULL
  set <- function(matrix) {
    x <<- matrix
  }
  get <- function() {x}
  setInverse <- function(inverse) {
    cached_inverse <<- inverse
  }
  getInverse <- function() {
    cached_inverse
  }
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  cached_inverse <- x$getInverse()
  if (!is.null(cached_inverse)) {
    message("getting cached inverse")
    return(cached_inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}
##test
test_matric <- matrix(c(1, 2, 4, 7), nrow = 2)
cached_matric <- makeCacheMatrix(test_matric)
inv <- cacheSolve(cached_matric)
inv_re <- cacheSolve(cached_matric)
