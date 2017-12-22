## makeCacheMatrix caches the inverse of a matrix in a list. It also provides a method to retain a cached matrix from a list
## cacheSolve checks if the inverse of the given matrix X is already in the cache, otherwise it calculates the inverse
## Motivation for the code was given from coursera assignment on caching the mean

## makeCacheMatrix uses an invertible matrix as input and puts in a cache, or gets back a calculated inverse from the cache

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve checks if the inverse of the input matrix X is in the cache. If yes the inverse is taken from the cache otherwise it is calculated

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
