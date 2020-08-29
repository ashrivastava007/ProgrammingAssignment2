## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Developed by Ankur Shrivastava on 29/08/2020
## Below Function is used to set, get Matrix and further get and set the inverse os matrix as well

makeCacheMatrix <- function(x = matrix()) {
        mat <- NULL
        set <- function(y) {
                x <<- y
                mat <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) mat <<- solve
        getinverse <- function() mat
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## Developed by Ankur Shrivastava on 29/08/2020
## Below Function is used to validate value of value of inverse of matrix in cache and if not then calculate the value of inverse of matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
