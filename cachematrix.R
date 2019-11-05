## Two functions that can brings a matrix into a cache to be
## checked if inversed then returned or computed inversed.
## makeCacheMatrix will take a matrix and inverse to cache.
## cacheSolve will solve a matrix if not inversed, or
## return through makeCacheMatrix (the cache) if already inversed.



## Makes a cache for the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    ## setting the matrix 
    m <- NULL
    setm <- function (y) {
        x <<- y
        m <<- NULL
    }
    
    ## getting the matrix
    getm <- function() x
    
    ## setting the the cache inverse
    setinvcache <- function(inverse) m <<- inverse
    
    ## getting the cache inverse
    getinvcache <- function() m
    
    ## defines the set, get, set, get 
    list(setm = setm, getm = getm, setinvcache = setinvcache, 
         getinvcache = getinvcache)
    
}

## Returns a matrix that is the inverse of 'x' 
## returns the 'cache' of the makeCacheMatrix if !null

cacheSolve <- function(x, ...) {
    
    ## sets m to getting the inversed cache matrix 
    m <- x$getinvcache()
    
    ## if matrix is not NULL return 'm' getting the cached data
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## Solves the matrix
    data <- x$getm()
    m <- solve(data, ...)
    
    ## sets the data to cache and returns 'm'
    x$setinvcache(m)
    return(m)
    
}

