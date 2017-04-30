## Matrix inversion is usually a costly computation and there may be some benefit to caching the
## inverse of a matrix rather than compute it repeatedly. This pair of functions can be used to
## cache the inverse of a matrix.
## Description taken from :
## https://www.coursera.org/learn/r-programming/peer/tNy8H/programming-assignment-2-lexical-scoping


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    # Assume that the matrix supplied is always invertible
    # Let's test that the object passed to this constructor is actually a matrix
    if (!(class(x) == class(matrix())))
    {
        warning("Must pass an object of class matrix")
        return(NULL)
    }
    
    # set the value of the matrix
    set <- function(y) {
        # Test if matrices are the same, from https://stat.ethz.ch/pipermail/r-help/2012-June/315408.html
        if (!(is.matrix(x) &&
              is.matrix(y) && dim(x) == dim(y) && all(x == y)))
        {
            if (is.null(inv))
            {
                message("dropping inverse as matrix appears to have changed")
            }
            inv <<- NULL
        } else
        {
            message("keeping inverse as matrix appears to be the same as before")
            # effectively performing this assignment
            # inv <<- inv
        }
        # Update the matrix assignment
        x <<- y
    }
    
    # get the value of the matrix
    get <- function()
        x
    
    # set the value of the inverse (not actually sure I want this method publically
    # exposed, but following the example.. hey ho)
    setinv <- function(solve)
        inv <<- solve
    
    # get the value of the inverse
    getinv <- function()
        inv
    
    #return value
    list(
        set = set,
        get = get,
        setinv = setinv,
        getinv = getinv
    )
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve
## should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    # Returns the inverse of x
    
    # Get the value of inverse from the `makeCacheMatrix` object`
    inv <- x$getinv()
    
    # check if value of inverse is cached ...
    if (!is.null(inv)) {
        message("getting cached value")
        return(inv)
    }
    
    # ... else calculate the inverse and cache it for next time
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    
    # Return the freshly calcualted inverse
    return(inv)
}
