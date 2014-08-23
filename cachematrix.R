## Matrix inversion is usually a costly computation so it is often valuable
## to cache the inverse of a given matrix rather than compute it
## repeatedly. This file implements a pair of functions:
## 
##   makeCacheMatrix:   This function creates a special matrix object that
##                      can cache its inverse.
##
##   cacheSolve:        This function computes the inverse of a special
##                      matrix returned by makeCacheMatrix(). If the
##                      inverse has already been calculated (and the
##                      matrix has not changed), then cachesolve
##                      retrieves the inverse from the cache instead
##                      of recomputing it.


## makeCacheMatrix
##
## This function creates a special matrix object that can cache its
## inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(inv) m <<- inv
    getsolve <- function() m
    list(set = set, get = get, setsolve = setsolve,
         getsolve = getsolve)
}


## cacheSolve
##
## This function computes the inverse of a special matrix returned by
## makeCacheMatrix(). If the inverse has already been calculated (and the
## matrix has not changed), then cacheSolve retrieves the inverse from the
## cache instead of recomputing it.

cacheSolve <- function(x, ...) {
    inv <- x$getsolve()
    if (!is.null(inv)) {
        message("getting the cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setsolve(inv)
    inv
}

