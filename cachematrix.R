# The function makeCacheMatrix creates a list
# to store a function to: 
# a) set the value of matrix
# b) get the value of matrix
# c) set the value of the inverse
# d) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) m <<- solve
	getsolve <- function() m
	list(set = set, get = get, setsolve = setsolve,
		getsolve = getsolve)
}


# This function checks wether the inverse of matrix has been
# computed and cached. If so, it returns the cached inverse. 
# Otherwise, it computes the inverse and stores it in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
