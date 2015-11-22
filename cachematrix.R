## Matrix with inverse cache support 

#' Creates a new matrix object with cache support. Matrix inverse cache is
#' initialized to null
#' 
#' @param x source matrix. If not provided, an empty matrix will be used.
#' @return a new Matrix object
#' @examples 
#' makeCacheMatrix() 
#' makeCacheMatrix(matrix(1:9, 3, 3))
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inv) inverse <<- inv
  
  getInverse <- function() inverse
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


#' Computes Inverse of matrix. Will use cached value if present.
#'
#' @param ... arguments to solve method
#' @param x Matrix object whose inverse is to be calculated
#' @return Inverse of matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # cache miss, compute matrix inverse and cache result
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
  }
