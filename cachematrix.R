## The makeCacheMatrix function makes a special object that stores a matrix. The cacheSolve function caches the object's (ie. the matrix's) inverse.

## The makeCache matrix creates a list containing various functions elaborated below

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	
	## The set function sets the value of the matrix
	
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	
	## The get function gets the value of the matrix
	
	get <- function() x
	
	## The setinv function sets the value of the matrix's inverse
	
	setinv <- function(inv) m <<- inv
	
	## The getinv function gets the value of the matrix's inverse
	
	getinv <- function() m
	
	## The list is created
	
	list (set = set, get = get,
			setinv  = setinv,
			getinv = getinv)
	
}


## The cacheSolve function first checks if the inverse has already been calculated. If so, it gets the inverse from the cache. If not, it calculates the inverse of the matrix and sets the value o the inverse via the setinv function.

cacheSolve <- function(x, ...) {
	
	## The portion below checks if the inverse has already been calculated. If so, the inverse is retrieved from the cache.
	
	m <- x$getinv()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	
	## If not, the inverse is calculated and the setinv function assigns the value of the inverse to m
	
	data <- x$get()
	m <- solve(data, ...)
	x$setinv(m)
	m
}
