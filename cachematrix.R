## Put comments here that give an overall description of what your
## functions do:

## The functions below are intended to reduce the strain associated with the inversion of a matrix
## multiple times. While with a small matrix the strain on computational time might be insignificant, with larger
## matrices this could have a major impact. Thus, it would be nonsensical to call the inverse of the same matrix 
## multiple times throughout your code. The functions below solves this by commiting a matrix to cache, which allows
## the already inverted matrix to be called at anytime with out having to execute the entire enversion function solve() again.

## Write a short comment describing this function

## The makeCacheMatrix contains a number of functions that are responsible for
## setting and getting both the value and the inverse of a provided matrix.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## This function will establish if the inverse of the matrix in question exists by triggering the getinverse() function, 
## if it does exist it will return it from the cache, if not it will inverse it using the solve function
## then it will commit the new inverse to cache through the setinverse() function. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
