## Put comments here that give an overall description of what your
## functions do

## Declare the function with a parameter called x of type matrix
## initializes Value of inversa to null
## Declare the intern function set that assigns the new value of the matrix
##   and initializes the dependant inverse matrix storing them in another space
## Declare function "get" that gives back the matrix assigned
## Declare the function "setInversa" that stores de inverse of the matrix
## Declare the function "getInversa" that returns the inverse
## Create the list of functions aviable in makeCacheMatrix function

makeCacheMatrix <- function(x = matrix()) {
    inversa <- NULL
    set <- function(mat) {
        x <<- mat
        inversa <<- NULL
    }
    get <- function() x
    setInversa <- function(InvCalc) inversa <<- InvCalc
    getInversa <- function() inversa
    list(set = set, get = get, setInversa = setInversa, getInversa = getInversa)
}


## Declares de cacheSolve function with parameter x (the matrix created to cache) and any needed arguments
## Assign the inverse to "mInv" reading it from the "object" create with the function makeCacheMatrix
## Verify if that the inverse is assigned (diferent of null) the notify the user that inverse matrix is getting from 
## the cache.
## On another case obtain the matrix, 
## calculate the inverse
## Cached the inverse
## return it to the user.

cacheSolve <- function(x, ...) {
    mInv <- x$getInversa()
    if (!is.null(mInv)) {
        message("getting cache data")
        return(mInv)
    }
    data <- x$get()
    mInv <- solve(data, ...)
    x$setInversa(mInv)
    mInv
}
