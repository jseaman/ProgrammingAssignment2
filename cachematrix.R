## The objective of this functions is to create a special structure which is
## able to store a matrix and cache the value of its inverse the first time
## it is needed. From then on as long as the values of the elements of the matrix
## remain the same there is no need to recompute its inverse.

## "makeCacheMatrix" : creates a special matrix that includes the value of the original matrix
## itself and the value of its inverse. The functions returns a list of functions
## that set and get the value of the matrix and its inverse respectively.

makeCacheMatrix <- function(x = matrix()) {
    ## initiate inv with a NULL value
    inv <- NULL
    
    ## set functions sets value y to x and sets inv to NULL again, 
    ## thus, the inverse will have to be recomputed and cached again
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # returns the matrix
    get <- function() x
    
    # sets the value of the inverse
    setInverse <- function(inverse) inv <<- inverse
    
    # gets the value of the inverse
    getInverse <- function() inv
    
    # #return a list with functions to get and set the matrix and its inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## "cacheSolve" : checks if the invere is already cached in memory. If it is the case
## this value is returned. If not, the inverse is calculated and stored in memory
## for future use.

cacheSolve <- function(x, ...) {
    ## retrieves the value of the inverse stored in the CacheMatrix object
    inv <- x$getInverse()
    
    ## First we check if the value is NOT null, meaning that it has been 
    ## calculated previusly. If that is the case, we return the value
    ## and we are done.
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## If we reached this point, it means that the inverse of the matrix
    ## has not been calculated yet. We need to retrieve the original matrix
    ## from the CaheMatrix object
    matrix <- x$get()
    
    ## The inverse is calculated. All additional parameters are passed to 
    ## the function
    inv <- solve(matrix, ...)
    
    ## We store the inverse for future uses
    x$setInverse(inv)
    
    ## the calculated inverse is returned once it has been stored
    inv
}
