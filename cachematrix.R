## The function makeCacheMatrix retuns an object that contains 4 functions 
## (set, get, setInverse and getInverse). These functions enable the object
## to cache the inverse of the given matrix when used with the CacheSolve function


## This function creates an object that can cache the the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  getInverse <- function() inv
  setInverse <- function(i) inv <<- i
  list(set=set, get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}


## Given a CacheMatrix object created by makeCacheMatrix this function
## will return the cached inverse of the stored matrix if it exists
## If the inverse has not been cached it will solve the inverse, store it in the
## makeCacheMatrix object, and finally return the inverse.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv))
  {
    message("getting cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}
