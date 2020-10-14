# The first function cache the information of a matrix and its inverse and the
# second function checks if a matrix has been inversed and retrieved the cached
# inversed matrix. If not, it computes the inverse of the matrix.

# makeCacheMatrix creates list that contains functions that:
# set value of the matrix (set())
# get the value of the matrix (get())
# set the value of the inversed matrix (setinv())
# get the value of the inversed matrix (getinv())

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(invMtrx) inv <<- invMtrx
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

a <- matrix(1:4, 2, 2)
inv_a <- makeCacheMatrix(a)

# cacheSolve is a function that checks the matrix returned by makeCacheMatrix if the 
# inversed matrix has been made. If it has been made it gets the cached inversed
# matrix in makeCacheMatrix. If it has not been made, it computes the inverse of
# the matrix and store it in the element "setinv" in the list made from 
# makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("Getting cached inversed matrix:")
                return(inv)
        }
        mtrx <- x$get()
        inv <- solve(mtrx)
        x$setinv(inv)
        inv
}

cacheSolve(inv_a)