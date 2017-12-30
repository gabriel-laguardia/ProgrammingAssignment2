##  This is a pair of functions that calculates and cache the inverse of a matrix.

## This function gives the methods for storing a matrix and its cached inverse

makeCacheMatrix <- function(x = matrix()) {
	
        ## Creates inv cleared
        inv <- NULL

        set <- function(newmatrix) {

                ## Sets new matrix
                x <<- newmatrix

                ## Clears inv if matrix changes
                inv <<- NULL
        }

        get <- function() x

        ## Cache the inv (but does not make the calculation)
        setinv <- function(inverse) inv <<- inverse

        ## Returns the inv cached (but does not make the calculation)
        getinv <- function() inv

        list(set = set, get = get,
                 setinv = setinv,
                 getinv = getinv)
}


## This function solves the inv of the given CacheMatrix if the value is not already cached

cacheSolve <- function(x, ...) {
	
        ## Returns the inv cached (or NULL)
        inv <- x$getinv()

        ## If cache is set
        if(!is.null(inv)) {

                message("getting cached data")
                return(inv)
        }

        ## Make the calculation
        matr <- x$get()
        inv <- solve(matr, ...)
        x$setinv(inv)
        inv
}
