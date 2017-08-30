
## 
## The two functions below allow a user to store a matrix, and compute
## and cache its inverse matrix.  Repeated accesses for the inverse
## matrix simply return the cached inverse, if it exists, so it is not
## computed multiple times.
##
## Usage example:
##     m <- matrix(c(1,2,3,4), 2,2)
##     mm <- makeCachedMatrix()
##     mm$set(m)
##     mm$getinv()   # returns NULL as no inverse has been cached yet
##     cacheSolve(m) # this computes and returns the inverse
##     mm$getinv()   # returns the cached inverse matrix
##     cacheSolve(m) # returns the cached inverse matrix
##
## (c) Sriram Darbha
##

## makeCacheMatrix() returns a special 'cached matrix' object, that
## is a list of functions to access a user-defined matrix and its
## inverse.  This can be called by providing a user-defined matrix,
## or alternately without args, in which case the user defined matrix
## can be set using $set() function.

makeCacheMatrix <- function(x = matrix()) {
    minv <- NULL
    set <- function(y) {
        x <<- y
        minv <<- NULL
    }
    get <- function() x
    setinv <- function(inv) minv <<- inv
    getinv <- function() minv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve() takes an instance of a 'cached matrix' (created by
## function above) and returns the inverse of the stored matrix.
## If inverse has been previously cached, it is returned.  Else,
## inverse is computed, cached and returned.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
