## https://github.com/murphytm/ProgrammingAssignment2
##
## cachematrix.R -- R Programming -- July 2014 -- Programming Assignment 2
##
## These functions facilitate the caching of the result of a potentially time-consuming matrix inversion.
## If cached, the inverted matrix can be looked up in the cache rather than recomputed.  The "<<-" operator
## is used to assign a value to an object in an environment that is different from the current.
##
## assumptions:
##    input matrix is always square and invertible
##
## operational example:
##    > m1 <- makeCacheMatrix(matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2))
##    > cacheSolve(m1)
##    > m1$get()
##    > m1$getinverse()




## This function creates a special "matrix" object that can cache its inverse.
##
## input:
##    a matrix (assumed square and invertible)
##
## output:
##    a list containing four functions (set, get, setinverse, getinverse)

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL

   set <- function(y) {                          ## function 1: set the value of the matrix
      x <<- y
      inv <<- NULL }

   get <- function() x                           ## function 2: get the value of the matrix

   setinverse <- function(solve) inv <<- solve   ## function 3: set the value of the inverted matrix

   getinverse <- function() inv                  ## function 4: get the value of the inverted matrix

   list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) }




## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.  If the inverse
## has already been calculated, it is retrieved from the cache and the computation is skipped.  Otherwise,
## the inverse is calculated and set in the cache.
##
## input:
##    a matrix (assumed square and invertible)
##    additional (and optional) arguments of the "solve" function
##
## output:
##    the inverse of the matrix

cacheSolve <- function(x, ...) {
   inv <- x$getinverse()                         ## try to get the inverted matrix from cache

   if(!is.null(inv)) {                           ## already cached ?  if yes, return the cached inverted matrix
      message("getting cached data")
      return(inv) }

   data <- x$get()                               ## get the matrix

   inv <- solve(data, ...)                       ## type "?solve" for additional arguments

   x$setinverse(inv)                             ## cache the inverted matrix

   inv }                                         ## return the newly computed inverted matrix
