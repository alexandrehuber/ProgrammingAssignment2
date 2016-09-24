## Functions to cache a matrix and its inverse (makeCacheMatrix) and to make the
## inversion with caching

## makeCacheMatrix returns a special matrix object (list of functions) based on 
## a matrix as input
## - Arguments
##      x: input matrix 
## - Output:
##      List of following functions
##      get(): a function that will retrieve the input matrix
##      set(): a function that will allow to set the matrix to be cached
##      getinverse(): a function that will retrieve the inverse matrix if it was
##            cached
##      setinverse(): a function that will set the cached value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    m <- NULL
    get <- function() x
    set <- function(m) {
        x <<- m
        i <<- NULL
    } 
    getinverse <- function() i
    setinverse <- function(n) i <<- n

    list(get = get, set = set, getinverse = getinverse, setinverse = setinverse)
}


## cacheSolve takes a special matrix object created by the makeCacheMatrix
## function and returns its inverse either from the cache or by calculating it.
## In the latter case it will store the inverse matrix within its cache
## - Arguments
##      x: makeCacheMatrix object containing the original matrix
##      ...: additionnal arguments for the solve function
## - Output: inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    m <- x$get()
    i <- solve(m, ...)
    x$setinverse(i)
    i
}
