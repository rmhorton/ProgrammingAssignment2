source("cachematrix.R")

set.seed(123)

my_cacheMatrix <- makeCacheMatrix( matrix( sample(1:16), nrow=4 ) )

my_cacheMatrix$getinverse()

Ainv <- cacheSolve(my_cacheMatrix)
A <- my_cacheMatrix$get()

round( A %*% Ainv )
round( Ainv %*% A )

my_cacheMatrix$getinverse()

cacheSolve(my_cacheMatrix)

