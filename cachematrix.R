## Creates a special "matrix" object that can cache its inverse
##
## Returns object containing few functions:
##  get() - returns matrix
##  set(y) - stores new matrix
##  setSolve(solve) - stores matrix's inverse
##  getSolve() - returns matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) inv <<- solve
    getSolve <- function() inv
    
    list(set = set, 
         get = get, 
         setSolve = setSolve, 
         getSolve = getSolve)
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    inv <- x$getSolve()
    if (!is.null(inv)) {
        message("getting cached solve")
        return(inv)
    }
    inv <- solve(x$get())
    x$setSolve(inv)
    inv
}
