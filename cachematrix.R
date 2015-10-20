## makeCacheMatrix takes one matrix argument, x. 
## Params: x - invertable matrix
## Returns: list object that is a cached representation of the 
##          original matrix. It has getters and setters for the 
##          original square matrix and for the inverse of this matrix
## Refer to inline comments for code explination:

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    ##Set initial matrix
    set <- function(y) { 
        ##cache matrix
        x <<- y
        ##don't calculate inverse until specifically called
        i <<- NULL
    }
    ##Get the original matrix
    get <- function() x
    ##Now cache the inverse of the matrix
    setInverse <- function(inverse) i <<- inverse
    ##Get the inverse of the original matrix
    getInverse <- function() i
    ##returns the list representation for the matrix
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve takes a 'makeCacheMatrix' object as a parameter
## and returns the inverse of this matrix. It also stores the inverse
## in cache thus for this matrix, when this operation is called again, 
## it does not need to re-calculate the result. 
## Params: 'makeCacheMatrix' matrix, other params that may be fed into solve method
## Returns: inverse of 'makeCacheMatrix' input
## See inline comments for more:

cacheSolve <- function(x, ...) {
    ##Check to see if the 'CacheMatrix' has an inverse saved already
    i <- x$getInverse()
    if(!is.null(i)) {
        ##If the inverse has previously been set then return that inverse
        return(i)
    }
    ##Alternatively get the matrix and solve for the inverse
    i <- solve(x$get(), ...)
    ##Save this as the inverse into Cache
    x$setInverse(i)
    ##Return the inverse
    i
}
