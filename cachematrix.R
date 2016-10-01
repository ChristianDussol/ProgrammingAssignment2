## Coursera R Programming course
# Week 3 programming assignment
#
# These following functions allow to manipulate matrix and its inverse
# As the matrix inverse computation can be costly, these following function manage
# the matrix inverse caching

# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  invMatrix <- NULL;
  
  set <- function(y) {
    x <<- y;
    invMatrix <<- NULL;
  }
  
  get <- function() x
  
  setInverseMatrix <- function(inverseMatrix) {
    invMatrix <<- inverseMatrix;
  }
  
  getInverseMatrix <- function() invMatrix
  
  list(set=set, get=get, setInverseMatrix=setInverseMatrix, getInverseMatrix=getInverseMatrix)
  
}


#  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  # Getting the cached inverse matrix if exists
  inv <- x$getInverseMatrix();
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # if there is no cached inverse matrix then computes the inverse and caches it
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverseMatrix(inv)
  inv
  
}
