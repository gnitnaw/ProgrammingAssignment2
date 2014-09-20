## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##  makeCacheMatrix(x=matrix()) : set the value of the matrix
##  makeCacheMatrix$set(x=matrix()) : set the value of the matrix
##  makeCacheMatrix$get() : get the value of the matrix
##  makeCacheMatrix$setSolve : set the Inverse of the matrix
##  makeCacheMatrix$getSolve : get the Inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) m <<- solve
        getSolve <- function() m
        list(set = set, get = get,
             setSolve = setSolve,
      	     getSolve = getSolve)
}


## Write a short comment describing this function
## cacheSolve first checks to see if the inverse has already been calculated. 
## Yes -> it gets the inverse from the cache and skips the computation. 
## No -> it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setSolve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getSolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setSolve(m)
        m
}
