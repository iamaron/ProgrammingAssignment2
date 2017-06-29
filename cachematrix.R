## Cache Matrix functions

## This function is used add new assignments in 
## a traditional matrix
## Ex: mtx <- makeCacheMatrix(matrix(c(1,2,3,4),c(2,2)))
## the mtx matrix has this functions below

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(mean) inv <<- mean
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function solve the mtx matrix
## Ex: cacheSolve(mtx)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
