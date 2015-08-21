## These functions create an object to store a matrix and to calculate and
## cache its inverse matrix. When the inverse matrix is calculated for the first
## time, it is stored in the object, and can be recalled rather than recalculated
## the next time its value is required.

## makeCacheMatrix instantiates an object containing the matrix and a list of functions
## to set and retrieve the matrix, and cache and retrieve its inverse

makeCacheMatrix <- function(x = matrix()) {
	inverse<-NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setinverse <- function(inv) inverse <<- inv
	getinverse <- function() inverse
	list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve operates on an objectcreated by the makeCacheMatrix function. If the object
## already contains a cached inverse matrix, the cached inverse will be returned. If there
## is no cached inverse, and inverse matrix is calculated using 'solve', and cached in the 
## object.

cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	if (!is.null(inv)){
		message("Getting cached matrix")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data,...)
	x$setinverse(inv)
	inv
}
