## The code is based on the example given "Caching the mean of a Vector"
##It is a pair of functions that cache the inverse of a matrix.


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        
        ## Initialize
        m <- NULL
        
        ## set the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## get the matrix
        get <- function() x
        
        ## set the inverse of the matrix
        setInverse <- function(inverse) m <<-inverse
        
        ## get the inverse of the matrix
        getInverse <- function() m
        
        ## return list
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        
        ## assign the inverse of the matrix
        m <- x$getInverse()
        
        ##If the inverse has already been calculated, retrieve the inverse from the cache
        if ( ! is.null(m)) {
                print("getting cached data")
                return(m)
        }
       
        ##Get the matrix
        data <- x$get()
        
        
        ## Calculate the inverse
        m <- solve(data, ...)
       
        ## set the inverse of the matrix
        x$setInverse(m)
        
        ## return matrix
        m
}
