## Return the inverse of a given matrix and cache the result

## function to store a given matrix and its calculated inverse

makeCacheMatrix <- function(x = matrix()) {
    # initialize inverse matrix
    inverseMatrix <- NULL
    
    set <- function(y) {
        x <<- y
        #reset inverse matrix when a (new) matrix is set
        inverseMatrix <<- NULL
    }
    get <- function() x
    
    setinverse <- function(calculatedInverse) inverseMatrix <<- calculatedInverse
    getinverse <- function() inverseMatrix
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Return the inverse of a matrix. 
## If the inverse was already computed, the stored result is returned

cacheSolve <- function(x, ...) {
    ## check if the inverse matrix is already calculated
    inverseMatrix <- x$getinverse()
    if(!is.null(inverseMatrix)) {
        message("getting cached data")
        ## Return a matrix that is the inverse of 'x' (stored matrix)
        return(inverseMatrix)
    }
    
    ## inverse matrix not calculated yet: 
    ## get the matrix and calculate the inverse matrix
    data <- x$get()
    inverseMatrix <- solve(data, ...)
    ## store the inverse matrix
    x$setinverse(inverseMatrix)
    
    ## Return a matrix that is the inverse of 'x'
    inverseMatrix
}
