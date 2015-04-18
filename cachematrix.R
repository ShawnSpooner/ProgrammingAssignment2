## Pair of functions to create a special matrix that can memoize its inverse

## Make cache creates a special matrix that allows the result of its inversion to be cached as a property
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        #the matrix has changed, invalidate the cached inverse
        m <<- NULL      
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Returns the inversion of the matrix x, uses the cached version if it is available
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("using memoized version of inverse")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
