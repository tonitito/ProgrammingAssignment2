##  This function caches the Matrix inverses which is a costly computation 
##  to provide some benefit by avoiding it to compute  repeatedly 


##  This function creates a special "matrix" object that can cache its inverse.
##  It contains the following functions:
##  1. set the value of the matrix X
##  2. get the value of the matix X
##  3. set the inverse of the matrix X
##  4. get the inverse of the matrix X


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverset = setinverse,
       getinverse = getinverse)
}


##  This function computes the inverse of the special "matrix" returned
##  by makeCacheMatrix above. 
##
##  If the inverse has already been calculated (and matrix not changed), 
##  then the cacheSolve retrieves the inverse from the cache.
##  If the inverse had not been calculated, then the cacheSolve compute
##  the inverse for square invertible matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  if (nrow(data)==ncol(data)){
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
  }else{
    message("Matrix is non-square. Inverse cannot be computed!")
  }
}

