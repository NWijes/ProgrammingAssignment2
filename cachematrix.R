## Assignment: Caching the Inverse of a Matrix
## Both makeCacheMatrix and cacheSolve functions are able to utilize caching
## inorder to avoid the painful and expensive task of repetitive matrix 
## inverse calculations.

## makeCacheMatrix creates a matrix object for its inverse that is able to 
## cache.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
  
	get <- function() x
	setInverse <- function(inverse) {
	inv <<- inverse
	}
  
	getInverse <- function() inv
	cacheInverse <- function() {
		if (!is.null(inv)) {
			message("Getting cached inverse...")
			return(inv)
		}
		message("Calculating inverse...")
		inv <<- solve(x)
		inv
		}
  
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse, cacheInverse = cacheInverse)
}



## cacheSolve function calculates the inverse of a matrix, checks if inverse
## has been calculated and cached. If it has, it returns the cached inverse and
## if it has not, it proceeds to calculate and cache then return the inverse. 


cacheSolve <- function(x, ...) {
	inv <- x$getInverse()
  	if (!is.null(inv)) {
		message("Getting cached inverse...")
		return(inv)
	}
  
	mat <- x$get()
	inv <- solve(mat, ...)
	x$setInverse(inv)
	inv
}
