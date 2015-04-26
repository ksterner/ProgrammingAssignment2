## This is an exercise to cache an inverted matrix and 
## retrieve it when it's available, instead of recalculating it
##
## Suggested test: 
## 
## > f <- matrix(runif(100), 10, 10)   # creates a 10x10 random matrix
## > g <- makeCacheMatrix(f)
## > cacheSolve(g)                     # solves the inverse and cache it
## > cacheSolve(g)                     # retrieves the inverse from cache
##

## https://github.com/ksterner/ProgrammingAssignment2/blob/master/cachematrix.R

## Setter and getter functions for the matrix and its inverse
## Returns a list of handles for the functions

makeCacheMatrix <- function(x = matrix()) {

	i <- NULL

	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinv <- function(inv) i <<- inv
	getinv <- function() i
	list(set = set,
	     get = get,
	     setinv = setinv,
	     getinv = getinv)

}


## cacheSolve consumes the list of handles and either
## solves the inverse or retrieves it if available

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	i <- x$getinv()
	if(!is.null(i)){
		message("getting cached matrix inversion")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinv(i)
	i
}

