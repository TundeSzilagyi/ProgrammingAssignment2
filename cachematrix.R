## There are two functions defined in this R file that allow the caching 
## and examination of a matrix inverse. At current there is no error handling 
## if the input matrix does not have a inverse.
##############################################################
## makeCacheMatrix() 
##------------------------------------------------------------
## FUNCTION that allows the caching of the inverse of the matrix and 
## sets up four functions associated with the input object.
## INPUT: invertible matrix.
## OUTPUT: object with associated four functions: set(), get(), setinv(), getinv()
## See below for the definition and explanation of these functions.
##############################################################
## cacheSolve() 
##------------------------------------------------------------
## FUNCTION checks whether the input object has an inverse assigned.
## If the inverse was not set then it calculates and pushes in the cache.
## INPUT: object defined with makeCacheMatrix() function.
## OUTPUT: inverse of the input object and makes sure that the inverse of the matrix
## object is set.
## ###########################################################
## Example of application:
## a <- makeCacheMatrix(matrix(1:4, 2, 2))
## a$getinv() 
## This is expected to return NULL.
## cacheSolve(a) ## This sets the inverse, because as shown above this was not shown.
## Return the inverse of the input 2 x 2 matrix, which here is [-2 1.5; 1 -0.5]
## -----------------------------------------------------------
## Any comments please address to:
## Tunde Szilagyi: a.t.szilagyi.coursera@gmail.com, 22.05.2014.

makeCacheMatrix <- function(x = matrix()) {
  ## Set the inverse of the matrix inverse to NULL 
  minv <-NULL
  ## Define the set function: able to pass on/set object properties
  set <- function(y){
    x <<- y
    minv <<- NULL
  }
  ## Define the get function that returns the input value of the object
  get <- function() x
  ## Set the inverse of the matrix inverse if called
  setinv <- function(solve) minv <<- solve
  ## Return the inverse of the input object if called upon
  getinv <- function() minv
  ## Return the list of four functions as assigned to the object
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Load the inverse if this exist for the object
  minv <- x$getinv()
  ## Check whether the content points to this inverse
  if(!is.null(minv)) {
    ## If the checked object is initilized with the inverse then return it
    message("getting cached inverse of the input matrix")
    return(minv)
  }
  ## If there is no inverse then load the input matrix data
  data <- x$get()
  ## Without amking any checks with respect the conditioning do the inverse
  minv <- solve(data, ...)
  ## set the inverse of the object using the object's function 'setinv'
  x$setinv(minv)
  ## return the matrix inverse
  minv  
}
