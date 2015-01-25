## These two functions are used to address costly
## computation of matrix inversion by caching
## previously computed results.


## This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solved) m <<- solved
    getsolve <- function() m
    list(set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve)
}


## This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve 
## retrieves the inverse from the cache.

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

## Execution results:
##
## > x=rbind(c(1, -1/4), c(-1/4, 1)) 
## > x
##       [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00
## > 
## > xC <- makeCacheMatrix(x)
## > cacheSolve(xC)
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## > cacheSolve(xC)
## getting cached data
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
