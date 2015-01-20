## The two functions below are used to create a special object
## that stores a matrix and cache's its matrix inverse.
## It is assumed that the matrix is invertible.


## Function makeCacheMatrix creates a special "matrix", which
## is really a list containing a function to:
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the value of the inverted matrix
##  4. get the value of the inverted matrix 

makeCacheMatrix <- function(x = matrix()) {
    sm <- NULL
    set <- function(y) {
        x <<- y
        sm <<- NULL
    }
    get <- function() x
    setsolve <- function(mx) sm <<- mx
    getsolve <- function() sm
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Function cacheSolve will calculate the inverse of the special
## "matrix" object. However, it first checks to see if the inverse
## has already been calculated. If so, it gets the mean from the
## cache and skips computation. Otherwise, it calculates the inverse
## of the matrix and sets the value of the cache via the setsolve
## function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    sm <- x$getsolve()
    if(!is.null(sm)) {
        #message("getting cached inverted matrix")
        return(sm)
    }
    data <- x$get()
    sm <- solve(data, ...)
    x$setsolve(sm)
    sm
}
