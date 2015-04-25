## These functions create an object containing a matrix and
## functions to a) store and read the matrix and b) calculate and
## cache the inverse of the matrix. The matrix is assumed to be invertible.

## Creates the matrix object with store/retrieve functions

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) { ## sets the matrix and clears the cache
        x <<- y
        inverse <<- NULL
    }
    get <- function() x  ## retreives the matrix
    setinverse <- function(solve) inverse <<- solve ## calculates the inverse
    getinverse <- function() inverse ## retreives the inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## checks for cached inverse ane returns cached data or calculates and
##  caches the inverse of the matrix

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {  ## checks for cached inverse
        message("getting cached data")
        return(inverse)
    }
    temp <- x$get()  ## retrieves the matrix and stores it in temp
    inverse <- solve(temp, ...)  ## calculates inverse matrix from temp
    x$setinverse(inverse)  ## calls setinverse to store inverted matrix
    inverse  ## returns calculated inverse
}
