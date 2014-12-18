## This is a pair of functions that would compute 
## and cache the inverse of a given matrix.

## 1st function creates a matrix as indicated and caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
        matrix <- NULL
        set <- function(y) {
                x <<- y
                matrix <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) matrix <<- solve 
        getinverse <- function() matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## 2nd function gets the inverse from cache 
## or computes it if none cache exists.

cacheSolve <- function(x, ...) {
        matrix <- x$getinverse()
        if(!is.null(matrix)) {
                message("getting cached data")
                return(matrix)
        }
        m1 <- x$get()
        matrix <- solve(m1, ...)
        x$setinverse(matrix)
        matrix
}
