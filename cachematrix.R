## Put comments here that give an overall description of what your
## functions do

## Creating a special matrix to cache the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }

    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m

    list (set = set, get = get,
            setinv = setinv,
            getinv = getinv)
}

## Calculate inverse of the matrix created in makeCacheMatrix

cacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinv()
        if(!is.null(m)) {
            message("Getting cached data")
            return(m)
        }

        data <- x$get()
        m <- solve(data)
        x$setinv(m)
        m
}
