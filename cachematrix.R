## Provides functions to calculate the inverse of a matrix once and
## stores the resultant inverse matrix in an object together with the original 
## matrix itself.
## This provides a cache mechanism such that if the inverse of a matrix 
## were to be required several times during processing, the inverse would only
## be computed once, enabling faster overall processing.

## This function creates a special "matrix" object that can cache its inverse.
## Assumption: Matrix passed in as argument must be invertible.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix ## has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s		
}
