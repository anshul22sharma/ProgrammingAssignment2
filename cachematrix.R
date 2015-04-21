## A pair of functions that cache the inverse of a matrix rather than computing it repeatedly

##"makeCacheMatrix" function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        y <- list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
	  return(y)
}


## "cacheSolve" function computes the inverse of the special matrix returned by function makeCacheMatrix

cacheSolve <- function(x) {

        ## Return a matrix that is the inverse of matrix 'x'       
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()

        m <- solve(data)
        x$setsolve(m)
        m
}


