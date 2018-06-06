## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix, that is a effectively a list containing 
## functions to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL                         # Defines m, being m the inverse of the matrix
    set <- function(y) {
        x <<- y                       # Sets the vaue of the matrix
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(x) m <<- x #Sets the inverse of the matrix
    getinverse <- function() m        #Returns the inverse of the matrix
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# The following function calculates the inverse of the special matrix
# created with the above function. However, it first checks to see if the
# inverse has already been calculated. If so, it get`s the inverse from the
# cache and skips the computation. Otherwise, it calculates the inverse of
# the matrix and sets the value of the inverse in the cache via the `setinverse`
# function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()          # Gets the inverse of the matrix, even if ths is still NULL
    if(!is.null(m)) {            # Checks if the inverse of the matrix has already been calculated
        message("getting cached data")
        return(m)                # Returns the inverse of the matrix
    }
    data <- x$get()
    m <- solve(data, ...)        # Calculates the inverse of the matrix
    x$setinverse(m)              # Sets the inverse of the matrix
    m                            # Returns the inverse of the matrix
}
