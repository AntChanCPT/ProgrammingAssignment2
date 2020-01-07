## These functions find the inverse of a matrix, assuming that the matrix is always invertible.
## One function can cache this inverse while the second will call upon it.
## If no cached inverse is found, the second function will calculate it.

## makeCacheMatrix creates a special matrix, which is a list containing functions to get and set the values of the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## cacheSolve calls the cached inverse matrix from makeCacheMatrix. If unavailable, this will inverse the matrix itself.

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
    ## Return a matrix that is the inverse of 'x'
}
