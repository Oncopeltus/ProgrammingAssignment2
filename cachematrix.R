## These two functions aim at reducing the cost of computation of inversing a matrix by cache the inverse.

## This function creates a matrix object (or consider it as a "class" if you know python) that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
                m <- NULL
                set <- function(y) {
                x <<- y
                m <<- NULL
                }
                get <- function() {x}
                setsolve <- function(solve) {m <<- solve}
                getsolve <- function() {m}
                list(set = set, get = get,
                setsolve = setsolve,
                getsolve = getsolve)
}


## This function computes the inverse of the matrix class returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then this function should retrieve the inverse from the cache.

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
