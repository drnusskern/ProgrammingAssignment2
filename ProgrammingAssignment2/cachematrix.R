#-- Creates a list containing the functions to set and get
#-- the values of the original matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
	im <- NULL
		#-- make the matrix and the cached inverse
		#-- available beyond the defining env
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) im <<- solve
	getinverse <- function() im
	list(set = set, get=get,
	     setinverse = setinverse,
	     getinverse = getinverse)
}


#-- Checks if the inverse of the original matrix has been
#-- calculated and returns if it exist, otherwise calculates
#-- and caches it for future use

cacheSolve <- function(x, ...) {

	im <- x$getinverse()
	if(! is.null(im)){
		message ("getting cached inverse")
		return(im)
	}
	mat <- x$get()
	
	#-- check that the original is a square matrix
	if(nrow(mat) != ncol(mat)){
		message ("matrix is not square:")
		return(mat)
	}
	#-- get the inverse 
	im <- solve(mat)
	x$setinverse(im)
	im
}
