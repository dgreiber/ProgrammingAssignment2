## Below are two functions that are used to create a special object that stores
## a numeric vector and cache's its mean.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    i  <- NULL  ## stores the inverse
    set  <- function(y){
        x <<- y
        i <<- NULL 
    }
    get  <- function() x  ## gets the matrix
    setinverse  <- function(inverse) i  <<- inverse
    getinverse  <- function() i
    list(set= set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i  <- x$getinverse()
    if (!is.null(i)){
        message("getting cached data")
        return(i)
    }
    data  <- x$get()
    i  <- solve(data, ...)
    x$setinverse(i)
    i
}
