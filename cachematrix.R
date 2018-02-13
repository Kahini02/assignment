## Here we are writing a function for cache matrix funtion,which can store the original matrix and get the inverse once the function is run. 

makeCacheMatrix <- function(x = matrix()) 
{
  invMat <-NULL
  set <- function(y)  ##set function
  {
    x <<- y
    invMat <- NULL
  }
  get <- function()x
  setinv <- function(inverse)invMat <<- inverse
  getinv <- function()invMat
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## Inverse of Special matrix created through the above function.

cacheSolve <- function(x, ...) 
{
  invMat <- x$getinv()
  if(!is.null(invMat))
  {
    message("Getting cached data")
    return(invMat)          ## Return a matrix that is the inverse of 'x'
  }              
    data <- x$get()   
    invMat <- solve(data)
    x$setinv(invMat)
    invMat
}
