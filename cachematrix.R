## Put comments here that give an overall description of what your
## functions do

## These functions were written to satisfy the requirements of
## assignment 2 for the R programming course

## Write a short comment describing this function
## The first function "makeCacheMatrix" creates a special "matrix"
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ##this creates the list of functions
    ivr <- NULL
    set <- function(y) {
        x <<- y
        ivr <<- NULL
    }
    get <- function () x
    setmatrix <- function(inverse) ivr <<- inverse
    getmatrix <- function() ivr
    list(set = set, get = get, setmatrix = setmatrix,
         getmatrix = getmatrix)
}


## Write a short comment describing this function
## The second function "cacheSolve" computes the inverse of the
## special "matrix" returned by the "makeCacheMatrix" above.  If the
## inverse has already been calculated (and the matrix has not changed),
## then "cacheSolve" will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    ivr <- x$getmatrix()
    if(!is.null(ivr)){
        message("getting cached matrix")
        return(ivr)
    }
    mat <- x$get()
    ivr <- solve(mat, ...)
    x$setmatrix(ivr)
    return(ivr)
    print(ivr)
}
