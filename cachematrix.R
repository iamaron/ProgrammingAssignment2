## Cache Matrix functions

## This function is used add new assignments in 
## a traditional matrix.

## set(matrix): store the matrix in the structure
## get(): get the matrix from the structure
## setinv(inverse): set the inverse matrix in the structure
## getinv(): get the inverse matrix from the structure

## Ex: mtx <- makeCacheMatrix(matrix(c(1,2,3,4),c(2,2)))
## the mtx matrix has this functions below

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(matrix) {
    x <<- matrix
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function solve the M matrix by getting the
## inverse matrix from the structure, when the inverse
## is already solved, or solving the matrix and storing
## it, when the matrix is not solved yet.

## Ex: cacheSolve(mtx)

cacheSolve <- function(M, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- M$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- M$get()
  inv <- solve(data, ...)
  M$setinv(inv)
  inv
}
