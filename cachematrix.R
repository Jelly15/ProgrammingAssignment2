#######################################################################
# Second programming assignment: peer assessment
#######################################################################

## FUNCTION makeCacheMatrix
### Creates a special "matrix" which is a list containing:
### - a function to set the matrix (i.e. the values of its elements) -> set
### - a function to get the matrix  -> get
### - a function to set the inverse of the matrix -> setinverse
### - a function to get the inverse of the matrix -> getinverse

makeCacheMatrix <- function(X = matrix(nrow=0, ncol=0)) {
        I <- NULL # I will contain the inverse
        set <- function(Y) { # function to set the value of the matrix
                X <<- Y
                I <<- NULL  # if we reset the matrix, then we need to delete the previous computed inverse
        }
        get <- function() X # value of the last created matrix
        setinverse <- function(inverse) I <<- inverse # function to set the inverse once we know it
        getinverse <- function() I
        list(set = set, get = get, # list containing the various functions aforementioned
             setinverse = setinverse,
             getinverse = getinverse)
}


## FUNCTION cacheSolve
### Computes the inverse of the matrix created with the makeCacheMatrix function. 
### If the inverse has been previously computed, it gets the inverse from the cache and skips the computation part. 
### If not, it computes the inverse and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(X, ...) {
        I <- X$getinverse() # get the cache 
        if(!is.null(I)) { # if the cache exists
                message("getting cached data")
                return(I) # return the cache
        }
        data <- X$get() # if the cache does not exist, it gets the matrix
        I <- solve(data, ...) # and computes its inverse
        X$setinverse(I) # save the result in the cache
        I # return the inverse
}
