## Matrix inversion is usually a costly computation.This work can save time by caching the inverse of a
## matrix to avoid compute it repeatedly. I write a pair of functions that cache the inverse of a matrix.
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

## This function don't do any thing,but provide interface for cacheSolve function.
## It is like shell for matrix,and have four nested function for operating matrix and the inverse 
## of matrix.
makeCacheMatrix <- function(x = matrix()) {
  # x is matrix for function parameter.
  # m is the inverse of matrix. 
  m <- NULL
  #Set matrix and clean the inverse of matrix.
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #return matrix. 
  get <- function() x
  #Set the inverse of matrix for inverse parameter.
  setinverse <- function(inverse) m <<- inverse
  #return inversematrix
  getinverse <- function() m
  #list four nested function.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function use makeCacheMatrix function to cache matrix. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      
  m <- x$getinverse()
  ##check the inverse of matrix.If it is not null, this function return the inverse of matrix directly.
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## computes the inverse of the matrix by solve function and set the inverse of  matrix in special 
  ## matrix by makeCacheMatrix.
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  ## return  the inverse of  matrix.
  m
}
