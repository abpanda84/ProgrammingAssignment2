## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Create a Matrix  - If a matric is passed, store it. Else, create an empty one.
makeCacheMatrix <- function(x = matrix()) {
  ## Create a variable that stores the Inverse of the Matrix
  ## Initialise it to Null as the Inverse is not cached yet
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## Get the matrix that was used to create the calling object
  get <- function() { x }
  
  ## Set the inverse Cache variable
  setinv <- function(inv) { m <<- inv }
  
  ## Get the inverse Cache variable
  getinv <- function() { m }
  
  ## Return a list object with Functions as its elements. The calling object can refer them as <obj$Function>
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## When invoked, check if the inverse of the matrix exists already
## If it does exist, return the cached inverse. 
## Else, calculate the inverse of the matrix using the solve() function. Cache the inverse as cache so that it can be
## fethched the next time the same matrix is passed
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 
  m <- x$getinv()
  
  ## Check if the Inverse of the Matrix exists already
  if(!is.null(m)) {
    #Cahched Inverse found - Return the cached value
    message("getting cached data")
    return(m)
  }
  ##Inverse not found in the Cache
  ## First, get the matrix from the "List" object passed
  data <- x$get()
  ## Calculate the inverse of the matrix
  ## Assumption - The matrix is invertible
  m <- solve(data, ...)
  ## Cache it for reuse at a later point in time
  x$setinv(m)
  ## Print the inverse Matrix
  m
}
