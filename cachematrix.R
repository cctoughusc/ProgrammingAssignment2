## Matrix Inversion Caching:
## Matrix inversion is usually a costly computation and there may be some benefit to caching the 
## inverse of a matrix rather than compute it repeatedly. Below are a pair of functions that are 
## used to create a special object that stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invt <- NULL
  set <- function(y) {
    x <<- y
    invt <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) invt <<- inverse
  getInverse <- function() invt
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then it should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invt <- x$getInverse()
  if (!is.null(invt)) {
    message("getting cached data")
    return(invt)
  }
  mat <- x$get()
  invt <- solve(mat, ...)
  x$setInverse(invt)
  invt
}

## Trial
matrix1 <- makeCacheMatrix(matrix(1:4, 2, 2))
matrix1$get()
matrix1$getInverse()
cacheSolve(matrix1)
matrix1$getInverse()

matrix1$set(matrix(c(2, 2, 1, 4), 2, 2))
matrix1$get()
matrix1$getInverse()
cacheSolve(matrix1)
cacheSolve(matrix1)
matrix1$getInverse()