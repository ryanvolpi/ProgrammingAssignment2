## These function enable the user to repeatedly recall the inverse of a vector by caching the reult after the first run so that as long as 
## the  vector doesn't change, the mean can be found afterward merely by drawing from the cahce

## makeCacheMatrix creates a list containing four functions: set, get, setInv, and getInv, which enable cacheSolve to cache the answer for 
## the inverse of matrix x

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInv <- function(inv) i <<- inv
  getInv <- function() i
  list( set = set,
        get = get,        ##returns the vector 
        setInv = setInv,  ##saves the calculated value of mean to a Universal variable called m
        getInv = getInv)  ##returns the value of m
}


## cacheSolve retrieves the value of i and checks to see if it is null. If it is, then the inverse has not yet been cached and the program
## must calculate the inverse. However, if i is not null, then cacheSolve can simply return the value of i, which is the cached value of the
## inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInv(i)
  i
}
B<-matrix(c(1,2,3,0,1,4,5,6,0),nrow=3,ncol=3)
b<-makeCacheMatrix(B)
cacheSolve(b)