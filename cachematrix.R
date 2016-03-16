## Cache the inverse of matrix.

## This function creates four functions of setting or getting a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function returns the inverse of the matrix input to makeCacheMatrix.
## The argument x is the list created by makeCacheMatrix.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached inverse")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
        ## Return a matrix that is the inverse of the input matrix to makeCacheMatrix.
}
