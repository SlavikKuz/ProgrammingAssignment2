## Functions for matrix inversion and caching rather than computing it repeatedly

## This function creates matrix that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
                }
        get <- function() x
        setmtx <- function(solve) m <<- solve
        getmtx <- function() m
        list(set = set, get = get, setmtx = setmtx,
             getmtx = getmtx)
}


## Computes the inverse of matrix returned by function above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmtx()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
                }
        data <- x$get()
        m <- solve(data, ...)
        x$setmtx(m)
        m
}
