## The following pair of functions are used to cache the inverse of an invertible matrix.

## The first function, makeCacheMatrix, takes a matrix and creates a matrix object that can
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {                    ## takes an input matrix, saves the matrix and reset inverse to NULL
		x <<- y
		m <<- NULL
	}
	get <- function() x                     ## returns the value of the input matrix
	setinv <- function(solve) m <<- solve  	## called by cacheSolve() during the first cacheSolve() access
	getinv <- function() m                  ## return the cached value to cacheSolve() during subsequence accesses
	list(set = set,
	     get = get, 
	     setinv = setinv,
	     getinv = getinv)
}


## The second function, cacheSolve, computes the inverse of the matrix returned by 
## makeCachMatrix. If the inverse has already been calculated (and the matrix) has
## not changed, then it retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
	if(!is.null(m)) {
		message("getting cached data")
		return(m) 
	}
	## returns the message "getting cachced data" if inverse has been calculated
	## (and not null), otherwise proceed to calculate the inverse
	mat <- x$get()
	m <- solve(mat, ...)
	x$setinv(m)
	m 
}
