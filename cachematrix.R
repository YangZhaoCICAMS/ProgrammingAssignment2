@@ -0,0 +1,45 @@
## Caching the Inverse of a Matrix

## The first function, makeCacheMatrix creates a special "matrix", which is really a list contains function to
##      1. set the value of the "matrix"
##      2. get the value of the "matrix"
##      3. set the inversed value of the "matrix"
##      4. get the inversed value of the "matrix"

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL 
        setmatrix <- function(y){
                x <<- y
                m <<- NULL
        }
        getmatrix <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(setmatrix = setmatrix,
             getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function get the inversed calue of the special "matrix" created with the above function.
## However, it first checks to see if the inversed value of the "matrix" has already been get. If so, it gets 
## the inversed value from the cache and skps thr computation. Otherwise, it get the inversed value of the "matrix"
## and sets the inversed value of the "matrix" in cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cache data")
                return(m)
                }
                z <- x$setmatrix()
                m <- solve(z, ...)
                x$setinverse(m)
                m
}



