## The functions makeCacheMatrix and cacheSolve use lexical scoping 
## to cache the inverse of a supplied matrix x and use
## this cached result in further operations (when available) to avoid having to 
## recalculate the inverse matrix each time it is required.


## Write a short comment describing this function
s
## makeCacheMatrix is a "wrapper function" which uses
## the cacheSolve function to
## provide an interface to allow a user to set and get
## the contents of a matrix and its inverse. 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    ## Setting matrix to a new value => set the matrix inverse (m) to null
    ## and assign input y to variable x in parent
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,setinverse = setinverse, getinverse = getinverse)

}


## Write a short comment describing this function

## cacheSolve returns the inverse of the supplied
## matrix x. It is assumed that x is invertible.
## The function returns a cached version of the inverse matrix (m) if available
## otherwise it calculates the inverse matrix using the built-in R function solve  
## and saves it to the global variable x for future use.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## first check if a cached inverse of x is available and if so
  ## return it. Otherwise, calculate the inverse of x and store this
  ## result with x
  m <- x$getinverse()
  if(!is.null(m)) {
    ## cached inverse is available
    message("getting cached data")
    return(m)
  }
  
  ## no cached inverse available therefore load the matrix x into data
  ## create the matrix inverse of x using the solve function, store this
  ## result in x for future use and return the inverse matrix m
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
