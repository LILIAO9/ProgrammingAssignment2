#### These functions are designed to cache the inverse of a matrix
#### So that to decrease the time for running the code

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  #set the stored value to null
  i <- NULL         
  #set value of the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  #get the value of the matrix
  get <- function() x
  #set the inverse of the matrix
  setinverse <- function(inverse) i <<- inverse
  #get the inverse of the matrix
  getinverse <- function() i
  #return all functions as a list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
  cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get() 
    i <- solve(data, ...)
    x$setinverse(i)
    i
  }
