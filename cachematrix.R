## The functions written below cache a matrix and its inverse
## to a local environment.

## NOTE: RUN THE FOLLOWING AT THE COMMAND LINE TO TEST:
## amatrix <- makeCacheMatrix(matrix(c(2,1,-1,2),2,2))
## amatrix$get() (comment: returns matrix stored in object "amatrix")
## cacheSolve(amatrix) (comment: runs and caches the inverse of the matrix stored in 
## object "amatrix)
## b <- cacheSolve(amatrix) (comment: R will tell us that b is assigned the cached 
## value of the inverse)
## amatrix$getinverse() (comment: returns cached inverse of matrix stored in object 
## "amatrix")



## The first function: makeCacheMatrix(), returns a list of four functions that can
## be assigned to a variable. Note that makeCacheMatrix does not 
## return an error if the input matrix is not invertible.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## This function: cacheSolve(), produces the inverse 
## of the matrix called in makeCacheMatrix(). Note that
## this function will produce an error if it tries to pass
## a singular or non-square matrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
