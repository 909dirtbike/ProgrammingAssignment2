# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a "matrix" which is really a list containing a 
# function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        invM <- NULL
        set <- function(y) {
                x <<- y
                invM <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invM <<- inverse
        getinverse <- function() invM
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The function below returns the inverse of the "matrix" created by the
# first function. It first checks to determine if the inverse has already 
# been calculated. If so, it gets the result from cache and skips the
# computation. Else, it computes the inverse, sets the value in the cache via
# setinverse function.  The function assumes that the matrix is always 
# invertible.
cacheSolve <- function(x, ...) {
        invM <- x$getinverse()
        if(!is.null(invM)) {
                message("getting cached data.")
                return(invM)
        }
        d <- x$get()
        invM <- solve(d)
        x$setinverse(invM)
        invM
}
