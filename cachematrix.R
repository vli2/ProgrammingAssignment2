#################################################################
#  Data Science: R Programming
#  Programming Assignment 2
#################################################################
# This assignment requires to write an R function that is able to 
# cache potentially time-consuming computations.  For example, 
# taking the inverse of a matrix is typically a fast operation.
# However, for a very large matrix, it may take too long to compute
# the inverse, especially if it has to be computed repeatedly 
# (e.g. in a loop).  If the contents of a matrix are not changing, 
# it may make sense to cache the value of the inverse so that when 
# we need it again, it can be looked up in the cache rather than 
# recomputed. In this Programming Assignment we will take advantage 
# of the scoping rules of the R language and how they can be manipulated 
# to preserve state inside of an R object.

#######################################################
#  Function: makeCacheMatrix
#######################################################
# This function creates a special "matrix" object 
# which can cache its inverse and is really a list 
# containing a function to 
# 
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse
#

makeCacheMatrix <- function(x = numeric()) {
	s <- NULL
	set <- function(y) {
		x <<- y
		s <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) s <<- solve
	getsolve <- function() s
	list(set = set, get = get,
		setsolve = setsolve,
		getsolve = getsolve)
}

##########################################################
# Function: cacheSolve
##########################################################
# This function computes the inverse of the special "matrix" returned by
#  makeCacheMatrix above.  If the inverse has already been calculated
# (and the matrix has not changed), then cacheSolve will retrieve the 
# inverse from the cache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
	s <- x$getsolve()
	if(!is.null(s)) {
		message("getting cached data")
		return(s)
	}
	data <- x$get()
	s <- solve(data, ...)
	x$setsolve(s)
	s
}		

