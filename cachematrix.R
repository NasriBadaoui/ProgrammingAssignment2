## Assuming that the matrix is invertible, this pair of function caches the inverse of a Marix

## The first function, makeVector creates a special "vector", which is really a list containing a function to set & get the value of the Matrix
## and also set and get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set <- function(y){
    x<<-y
    i<<-NULL
  }
  get <- function() x
  setsolve <- function(sovle) i <<- inverse
  getsolve <- function() i
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}


## The following function calculates the mean of the special "vector" created with the above function.
## However, it first checks to see if the mean has already been calculated.
## If so, it gets the mean from the cache and skips the computation. 
## Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.

cacheSolve <- function(x, ...) {
          
    i <- x$getsolve()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
  
    data <- x$get()
    i <- solve(data, ...)
    x$setsolve(i)
    i
        ## Return a matrix that is the inverse of 'x'
}
