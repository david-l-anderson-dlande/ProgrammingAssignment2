## This pair of functions first creates a data structure in the form of 
## a list of functions caching and returning a matrix and its inverse.
## The second function returns or, if there is none, sets the inverse
## in one of these data structures.




## Creates a list of four functions, which can be used to respectively:
## Cache (set) a matrix
## Return (get) the cached matrix
## Cache (setinv) the inverse of the matrix
## Return (getinv) the cached inverse

makeCacheMatrix <- function(thematrix = matrix()) {
    theinverse <- NULL
    
    ## this stores inputmatrix in this data structure
    ## and sets theinverse to NULL
    ## there is no check that the input is actually a matrix
    set <- function(inputmatrix) {
        thematrix <<- inputmatrix
        theinverse <<- NULL
    }
    
    ## run get to see the stored matrix
    get <- function() thematrix
    
    ## sets the inverse of the matrix; does not check whether it is correct
    setinv <- function(inverse) theinverse <<- inverse
    
    ## returns the inverse of thematrix, if defined
    getinv <- function() theinverse
    
    ## definition of the elements in the returned list
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}




## Returns the inverse of amatrix, which needs to be the data structure
## created by makeCacheMatrix.
## Looks up the inverse of amatrix, and if null, calculates it.

cacheSolve <- function(amatrix, ...) {
    ## looks up the inverse
    inv <- amatrix$getinv()
    
    ## returns the cached inverse if it exists
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## if the inverse doesn't exist, calculate it
    data <- amatrix$get()
    inv <- solve(data, ...)
    ## then set the inverse in the data structure
    amatrix$setinv(inv)
    
    ## return the inverse of amatrix
    inv
}