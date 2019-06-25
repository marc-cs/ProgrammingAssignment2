

## Function that creates a special matrix object that can cache its inverse
## Steps:
## 1 - set the value of the vector
## 2 - get the value of the vector
## 3 - set the value of the mean
## 4 - get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  
  # initializationm of the matrix
  
  m <- NULL
  
  # set the value of the matrix
  
  setMatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # get the value of the matrix
  
  getMatrix <- function() x
  
  # set the value of the inverse
  
  setInverse <- function(inverse) m <<- inverse
  
  # get the value of the inverse
  
  getInverse <- function() m
  
  # list the methods 
  
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Function that computes the inverse of the special matrix returned by makeCacheMatrix.
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # get the inverse of the matrix that is in cache
  m <- x$getInverse()
  
  # if it is not null return the result
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  
  # otherwise 
  # get the  matrix, , and 
  data <- x$getMatrix()
  
  # calculate the inverse
  inv <- solve(data,...)
  
  # set the value of the inverse in the cache
  x$setInverse(inv)
  
  #return the inverse
  inv
}
