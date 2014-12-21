## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	# same idea as the template, with solve instead of mean
	# solve returns the inverse, keep the names consistent with the fu
	setsolve <- function(solve) m <<- solve
	getsolve <- function() m
	list(set = set, get = get,
		setsolve = setsolve,
		getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getsolve()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	# following the same recipe as the template, based on the members of makeCacheMatrix
	data <- x$get()
	m <- solve(data, ...)
	x$setsolve(m)
	m
}
