##The makeCacheMatrix function takes as input a matrix object and outputs a list of the four functions get, set, getInverse and setInverse.
##This list of functions can be used to store and retrieve data about the matrix object of the makeCacheMatrix input.
##The cacheSolve function takes as input the output list of makeCacheMatrix and calls setinverse form makeCacheMatrix in order
##to set a value for the inverse of the original matrix.  This inverse solution is also returned as a result of cacheSolve.

##makeCacheMatrix creates a list of elements which can be used to store and access an input matrix and its inverse.
##The object m is set to NULL to allow for use of this internally in the "set" and "get" Inverse functions.
##The set() function (an element of the list output by makeCacheMatrix) can be used to override the existing data 
##used by the makeCacheMatrix funciton, this data is referred to internally in the function as "x".  
##Set includes a restriction (!is.matrix) that the input value must be a matrix, and will override the value
## of x in set's parent environment (makeCacheMatrix) through the super assignment operator, <<-.
##Similarly the value of m in the "set" and "makeCacheMatrix" environments is set to NULL, 
##effectively  resetting the values of the getInverse and setInverse functions when subsequently called. 
##The get() function simply returns the value of x, which is initially the input of the makeCacheMatrix parent function
## but may be superceded by the set() function.
##setInverse() is defined as a method to override any existing stored inverse with an input referred to in the function as "solve".
##There are no restrictions on setInverse so that any value may be used and that value with be subsequently returned through getInverse().
##getInverse() returns the value of the internal object "m" set by setInverse()
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    if(!is.matrix(y)) {
      "Please input a matrix"
      return(y)
    }
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse) 
}


##cacheSolve interacts with the functions defined for the list output of makeCacheMatrix to set a value for the Inverse of the 
##objective matrix from makeCacheMatrix.  By using cacheSolve() to solve for the inverse of the objective matrix cacheSolve
##saves resources by avoiding multiple solve() calls for the same matrix.  This is done by first testing whether the makeCacheMatrix list
##input already has a value for getInverse().  Since getInverse is defined as "m" in makeCacheMatrix and "m" is assigned NULL absent any
##call to setInverse any call with a value for getInverse will bypass the rest of cacheSolve and return the predefined value of the
##getInverse call to the input, stored as "m".  If getInverse() returns NULL then thbe function will continue, storing the objective matrix
##as "data" using the get() sub-function from the makeCacheMatrix list.  Since no inverse exists (e.g. getInverse is NULL) the 
##R solve() function is called and its result is assigned to "m".  This result (as "m") is then used as an input to setInverse()
##effectively storing the matrix inverse for later use.  Any subsequent calls to cacheSolve will return the value of getInverse()
##bypassing the solve() function and saving resources.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}