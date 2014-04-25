## This mimickes the example provided in the assignment instructions
##
## makeCacheMatrix provides an object with data and functions
## to store and retrieve a matrix and its inverse
##
## cacheSolve is a function operating on the kind of object created
## by makeCacheMatrix to compute the inverse of a matrix assumed to be
## invertible
##
## I didn't use Rstudio to edit and prototype the functions
## I preferred installing ESS (Emacs Speak Statistics) and editing the
## code with Emacs, that with the ESS addition recognizes the relevant
## R key words

## makeCacheMatrix builds an object out of a matrix, providing datastructures
## to store the matrix and its inverse and to set and retrieve them

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve operates on list objects created with makeCacheMatrix and
## returns the matrix inverse
## the latter is computed and set on the object when the function is called
## for the first time, otherwise it is retrieved from the object cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
