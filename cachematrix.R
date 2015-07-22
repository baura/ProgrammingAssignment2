## The idea behind is very similar to the example, just with a 
## 2-dim vector and a different function: solve.
## 

##  makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##  -set the value of the matrix
##  -get the value of the matrix
##  -set the value of the inverse matrix
##  -get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
  A <- NULL
  set <- function(y) {
        x <<- y
        A <<- NULL
  }
  get <- function() x
  setinv <- function(invers) A <<- invers
  getinv <- function() A
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated
##(and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache
##and print the string "getting cached data".

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  A <- x$getinv()
  if(!is.null(A)) {
      message("getting cached data")
      return(A)
  }
  data <- x$get()
  A <- solve(data, ...)
  x$setinv(A)
  A
}

