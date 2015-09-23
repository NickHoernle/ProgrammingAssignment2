## makeCacheMatrix takes one matrix argument, x. 
## Params: x - invertable matrix
## Returns: list object that is a cached representation of the 
##          original matrix. It has getters and setters for the 
##          original square matrix and for the inverse of this matrix
## Refer to inline comments for code explination:

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL # there is no inverse yet
    set <- function(y) {
        x <<- y # cache the matrix x
        i <<- NULL # for a new matrix, no inverse is cached
    }
    get <- function() x  # return the matrix
    setInverse <- function(inverse) i <<- inverse # set the inverse, save to cache
    getInverse <- function() i # get the inverse (will return null if not set)
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse) #return the list representation for the matrix
}


## cacheSolve takes a 'makeCacheMatrix' object as a parameter
## and returns the inverse of this matrix. It also stores the inverse
## in cache thus for this matrix, when this operation is called again, 
## it does not need to re-calculate the result. 
## Params: 'makeCacheMatrix' matrix, other params that may be fed into solve method
## Returns: inverse of 'makeCacheMatrix' input
## See inline comments for more:

cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if(!is.null(i)) { # if inverse has previously been set then return that inverse from cache
        return(i) # returned cached inverse
    }
    thisMatrix <- x$get() #alternatively get original matrix
    i <- solve(thisMatrix, ...) #determine the inverse
    x$setInverse(i) # set the inverse of the matrix into cache (to possibly be called later)
    i # return the inverse
}
