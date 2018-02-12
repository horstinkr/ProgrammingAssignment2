## Programming Assignment 2 for Reinoud Horstink - reinoud.horstink@arcadis.com
## Data science course, Programming R (week 3)

makeCacheMatrix <- function(x = matrix()) { 

	## This function creates a special "matrix" object that can cache its inverse
	## Assume that the matrix supplied is always invertible

	m <- NULL
	set <- function(y) {
		# the set function pushes the value of y to x and resets m to NULL if a new matrix is passed 
		x <<- y
		m <<- NULL
	} 
	get <- function() x
	# setmatrix is an anonymous function which replaces m with the inverse of m
	setmatrix <- function(solve) m <<- solve
	# getmatrix returns m, if it has been cached
	getmatrix <- function() m
	# create a makeCacheMatrix object with set, get, setmatrix, and getmatrix attributes 
	list(set = set, get = get,	
			setmatrix = setmatrix, 
			getmatrix = getmatrix)

}

cacheSolve <- function(x, ...) { 

	## Return a matrix that is the inverse of 'x'
	## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache.

	# retrieve and return the cached matrix if available
	m <- x$getmatrix()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	} 

	# calculate the inverse and return this inverse matrix
	data <- x$get()
	m <- solve(data, ...)
	x$setmatrix(m)
	m

}