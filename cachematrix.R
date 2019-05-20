## The functions below are for the purpose of caching the inverse of 
## a matrix rather. This can be easier than computing it repeatedly.

## makeCacheMatrix is a function that creates a matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        ## Makes an empty cache for the inverse matrix
        im <- NULL
        
        ## Sets data for the matrix (not the inverse) 
        ## Will automatically reset inverse cache to null
        setMatrix <- function(y) {
                x <<- y
                im <<- NULL
        }
        
        ## Gets the data of the matrix
        getMatrix <- function() x
        
        ## Sets data for the matrix inverse
        setInverse <- function(inverse) im <<- inverse
        
        ## Gets the data of the inverse matrix
        getInverse <- function() im
        
        ## Returns a list of our functions
        list(setMatrix = setMatrix, getMatrix = getMatrix, 
             setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve is a function that computes the inverse of the matrix
## returned by makeCacheMatrix above. If the inverse is already solved then
## cacheSolve can use it from having been stored in the cache

cacheSolve <- function(x, ...) {
        ## Gets the inverse matrix
        im <- x$getInverse()
        
        ## If inverse matrix is not null then return the cache
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        
        ## Gets the matrix and solves it's inverse
        data <- x$getMatrix()
        im <- solve(data, ...)
        
        ## Stores the inverse matrix in the cache and returns it
        x$setInverse(im)
        im
}
