## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## 1st function creates a matrix and keeps space for inverse matrix storing

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        ## Setting Matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## Getting the Matrix returned
        get <- function() x
        ## Setting inverse of Matrix
        setinverse <- function(solve) m <<- solve
        ## Getting stored inverse of InitialMatrix
        getinverse <- function() m
        ## list of all functions
        list (set = set, get = get, 
              setinverse = setinverse, 
              getinverse = getinverse)
}


## Write a short comment describing this function
## 2nd function solves given matrix to find the inverse. 
## First trying for stored inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## ask x wether it already has an inverse stored.
        m <- x$getinverse()
        ## if already stored signal that using stored and return the inverse
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## otherwise get the matrix and solve it
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
