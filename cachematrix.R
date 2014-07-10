## This is a pair of functions to create a "CacheMatrix" object
## and to take its inverse, while cacheing the result.


########################## makeCacheMatrix ##########################
## A factory function to generate a "CacheMatrix" object that can  ##
## cache its own inverse.                                          ##
##                                                                 ##
## Argument: x, a numeric matrix.                                  ##
## Returns: a CacheMatrix object encapsulating x.                  ##
##                                                                 ##
## The CacheMatrix object is implemented as a list of function     ##
## closures, all of which were defined in the environment of the   ##
## factory function, and can thus refer to objects in that         ##
## environment. The functions are:                                 ##
##     set: assigns its argument to x                              ##
##     get: returns x                                              ##
##     setinverse: assigns its argument to x_inv                   ##
##     getinverse: returns x_inv                                   ##
## Note that the actual inverting of the matrix is done in the     ##
## external function "cacheSolve", which uses the accessor         ##
## methods described above.                                        ##
#####################################################################

makeCacheMatrix <- function(x = matrix()) {
	x_inv <- NULL
	set <- function(y){
		x <<- y
		x_inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inv) x_inv <<- inv
	getinverse <- function() x_inv
	res <- list(set=set, get=get,
		setinverse = setinverse,
		getinverse = getinverse )
	class(res) <- "CacheMatrix"
	res
}

############################ cacheSolve #############################
## A helper function for CacheMatrix objects that computes the     ##
## inverse of the matrix held by the object and stores the result  ##
## back within the object. The inverse is computed the first time  ##
## the function is called, and subsequent calls return the cached  ##
## result.                                                         ##
##                                                                 ##
## Argument: x, a CacheMatrix object.                              ##
## Returns: the inverse of the CacheMatrix object x.               ##
## The first time this function is called, it calculates the       ##
## inverse of the matrix x (that is, it "solves" the matrix), and  ##
##                                                                 ##
## Note that the inverse uses the ginv function from the MASS      ##
## package, which is robust toward computational singularities.    ##
#####################################################################

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if ( !is.null(inv) ){
		message("getting cached result")
		return(inv)
	}
	matX <- x$get()
	# inv <- solve(matX)	# hope it's not singular
	require(MASS); inv <- ginv(matX)	# singular, schmingular
	x$setinverse(inv)
	inv
}
