## Put comments here that give an overall description of what your
## functions do

## Caching the Inverse of a Matrix

#This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(matinv) inv <<- matinv
  getinv <- function() inv
  list(set = set, get = get, 
       setinv = setinv, 
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by  
## makeCacheMatrix  above. If the inverse has already been calculated (and 
## the matrix has not changed), then  cacheSolve retrieves the inverse 
## from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    inv<-x$getinv
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat,...)
  x$setinv(inv)
  inv
}
