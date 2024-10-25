## This pair of functions allows caching of a matrix's inverse to avoid
## recomputing it. The 'makeCacheMatrix' function creates a special matrix
## object that can store its inverse, and 'cacheSolve' calculates the inverse,
## caching the result for future use.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## It contains four functions: set, get, setInverse, and getInverse. 
## - 'set' assigns a new matrix and resets the inverse cache.
## - 'get' retrieves the matrix.
## - 'setInverse' stores the inverse in the cache.
## - 'getInverse' retrieves the cached inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Initialize the inverse as NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL  # Reset the inverse cache when the matrix changes
    }
    get <- function() x  # Retrieve the matrix
    setInverse <- function(inverse) inv <<- inverse  # Cache the inverse
    getInverse <- function() inv  # Retrieve the cached inverse
    
    # Return a list containing all four functions
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## it retrieves the cached inverse to save computation time.
## If the inverse is not cached, it calculates it, caches it, and returns it.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()  # Check if inverse is already cached
    if (!is.null(inv)) {  # If cached, retrieve and return it
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()  # Retrieve the matrix if inverse is not cached
    inv <- solve(mat, ...)  # Calculate the inverse
    x$setInverse(inv)  # Cache the inverse
    inv  # Return the inverse
}
