## This code corresponds to the RDPeng R course from the Statistics toolbox exercice week 3
## This is an example of how to use the <<- operator in order to create a matrix that is able to cache its inverse.
## there are two functions provided makeCacheMatrix that initialises the object
## and cacheSolve that computes the inverse of the matrix.

## makeCacheMatrix creates a special "matrix" object
## that can cache its inverse.
## makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get      <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
        
}


## cacheSolve: This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$cacheSolve()
        if(!is.null(m)) {
                ##message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m 
}
