## These functions are used to work with a "matrix object" which
# contains a cached copy of its inverse.

## Create a "cache matrix object" which contains function to set and
## get the matrix, and set and get the cached inverse. If the matrix
## is modified, clear the cached inverse.

makeCacheMatrix <- function(x = matrix()) {
        cacheInv <- NULL
        set <- function(y) {
                x <<- y
                cacheInv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) cacheInv <<- inverse
        getinverse <- function() cacheInv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Given a "cached matrix object" get the inverse. It there is
## a cached inverse available use it, else compute the inverse
## and cache the result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        inv <- solve(x$get())
        x$setinverse(inv)
        inv
}
