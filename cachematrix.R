# Matrix inversion is usually a costly computation and there may be some benefit 
# to caching the inverse of a matrix rather than compute it repeatedly (there are 
# also alternatives to matrix inversion that we will not discuss here). These pair 
# of functions cache the inverse of a matrix.
# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


# This function computes the inverse of the special "matrix" returned 
# by makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve 
# the inverse from the cache. Computing the inverse of a square matrix 
# can be done with the solve function in R. For example, if X is a square 
# invertible matrix, then solve(X) returns its inverse.

# For this assignment, assume that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("Getting cached inverse...")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}

## Example:
## > x <- matrix(c(1,2,3,4), 2,2)
## > mtx <- makeCacheMatrix(x)
## > mtx$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## First call to the function with no cached data
## > cacheSolve(mtx)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(mtx)
## Second call with the inverse already cached
## Getting cached inverse..
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
