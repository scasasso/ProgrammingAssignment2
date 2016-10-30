## The following code allows to create a matrix object 
## which is able to cache the computation of the inverse
## thus avoiding unnecessary computations

## This function creates a list of "setters"
## and "getters" for a matrix object (passed as argument)

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## This functions computes the inverse of the matrix (passed as argument)
## if it has never been computed before. Otherwise it returns the cached result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if (!is.null(i)){
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data,...)
    x$setinverse(i)
    i
}
