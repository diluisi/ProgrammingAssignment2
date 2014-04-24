##Functions that cache the inverse of a matrix.

## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL  ##Cache a matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, ##Create a list to call get and set functions
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse() ##Search matrix
        if(!is.null(m)) { ##If cache is different of NULL
                message("getting cached data")
                return(m) ## Return cache
        }
        data <- x$get() ##No cache assigned
        m <- solve(data, ...) ##Calculate inverse
        x$setinverse(m) ##Set the matrix inverse and save the result x cache
        m ##Result
}
