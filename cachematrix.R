## These two functions are used to solve the Data Specialty
## Programming Assingment #2: Peer Assessment

## makeCacheMatrix creates a special matrix of objects
## designed to cache its inverse.  First it sets the value
## of the matrix. Then it gets the value of the matrix.
## It then sets the value of the inverse, and finally gets
## the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}

## This function calculates the inverse of the
## matrix created abov. To eliminate redundancy,
## it first checks if the inverse has already
## been solvrd. If it has, the function grabss
## the inverse from the cache and skips the computation.
## If the inverse wasn't solved, the function finds the
## inverse of the data and sets the value of the inverse
## in the cache through the setinverse function.

cacheSolve <- function(x, ...) {
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