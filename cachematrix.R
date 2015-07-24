## This two functions (should) reverse a matrix 
## but they use a trick: if the matrix was already 
## reversed before, the second function uses the cached 
## answer to save time. If not, it reverses the matrix normally

## The following function creates a list of functions
## to get matrix, set matrix, set inversed matrix and 
## get inversed matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() x
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## This function checks if the cached reversed matrix exists
## and if it does, returns the cached result. If it doesn't-
## it reverses the matrix the usual way

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
