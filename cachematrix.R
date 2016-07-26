## Matrix inversion can be a time-consuming computation and as a result it can be
## beneficial to cache the inverse of the matrix instead of repeatedly computing it.
## The functions below create a matrix that can cache its inverse and the either computes 
## the inverse of the matrix or retrieves it if it was already computed. 
## These functions assume that the matrix is always invertible.

## The following function creates a special matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        mi <- NULL
        set <- function(y) {
                x <<- y
                mi <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) mi <<- inverse
        getinverse <- function() mi
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function calculates the inverse of the special matrix created with the above function
## and sets the value of the inverse in the cache via the setinverse function.
## If the inverse has already been calculated, the function gets the inverse from the cache and skips the calculation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mi <- x$getinverse()
        if(!is.null(mi)) {
                message("getting cached data")
                return(mi)
        }
        data <- x$get()
        mi <- solve(data, ...)
        x$setinverse(mi)
        mi
}
