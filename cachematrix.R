
##makeCacheMatrix creates a special matrix, which is a list containing a function to:
###1: TO set the value of the matrix
###2: TO get the value of the matrix
###3: To set the value of the inverse of the matrix
###4: To get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
   get <- function() x
   setinver <- function(inverse) inver <<- inverse
   getinver <- function() inver
   list(set=set, get=get, setinver=setinver, getinver=getinver)
}


## The following function, cacheSolve, returns the inverse of the special matrix from CacheMatrix

cacheSolve <- function(x, ...) {
  inver <- x$getinver()
  if (!is.null(inver)) {
     message("getting cached data")
    return(inver)
  }
    data <- x$get()
    inver <- solve(data,...)
    x$setinver(inver)
    inver
}
        ## Return a matrix that is the inverse of 'x'

