## Programming Assignment 2 for Reinoud Horstink - reinoud.horstink@arcadis.com
## Data science course, Programming R (week 3)

makeCacheMatrix <- function(x = matrix()) {

	## This function creates a special "matrix" object that can cache its inverse
	## Assume that the matrix supplied is always invertible
	
	# actually, I've chosen to use a dataframe instead of a matrix
	# use <<- to bring this cache object to the global environment

	# xser: x serialized (as reference key)
	# xi:   inverse matrix of x
	
	mycachematrix <<- data.frame(xser="a", xi="b")

}


cacheSolve <- function(x, ...) {

	## Return a matrix that is the inverse of 'x'
	## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache.
	## xi: inverse of matrix x

	# serialize the original matrix as unique key for looking up the inverse
	xser <- serialize(x, NULL)

	if (!is.na(mycachematrix$xser[[xser]])) {
		# inverse does already exist in cache --> lookup inverse matrix in cache object
		xi <- mycachematrix$xser[[xser]]
	} else {
		# inverse does not yet exist in cache --> compute inverse matrix now and store it in mycachematrix
		xi <- solve(x, ...)
		mycachematrix[[xser]] <<- xi
	}
	
	# return inverse matrix
	xi

}