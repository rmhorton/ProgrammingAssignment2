## This is a pair of functions to create a "CacheMatrix" object
## and to take its inverse, while cacheing the result.

## Generate a "CacheMatrix" object that can cache its own inverse.

makeCacheMatrix <- function(x = matrix()) {
	my_inverse <- NULL
	set <- function(y){
		x <<- y
		my_inverse <<- NULL
	}
	get <- function() x
	setinverse <- function(inv) my_inverse <<- inv
	getinverse <- function() my_inverse
	list(set=set, get=get,
		setinverse = setinverse,
		getinverse = getinverse )
}


## Return the inverse of the CacheMatrix object x, and 
## cache the result within x. The first time this is called,
## it will compute the inverse of the matrix x. Subsequent
## calls will return the cached result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if ( !is.null(inv) ){
		message("getting cached result")
		return(inv)
	}
	matX <- x$get()
	inv <- solve(matX)	# I sure hope it is not singular
	x$setinverse(inv)
	inv
}
