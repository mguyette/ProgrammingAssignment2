## These two functions an be used to create an object that stores
## a matrix and caches its inverse

## This function creates a "matrix" object that can cache its
## inverse.  This "matrix" object is a list containing a function
## to set the value of a matrix, get the value of that matrix,
## set the inverse of the matrix, and then get the inverse of the
## matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" created
## by the makeCacheMatrix() function above
## Before computing the inverse of the matrix, it checks to see if
## the matrix inversion has already been completed.  If so, it uses
## the cached matrix. If not, it calculates the inverse of the matrix
## and stores the inverse

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
