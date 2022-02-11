## These functions work in conjunction to cache the inverse of a matrix after
## it has been calculated. This allows the program to reduce overall computing
## time by recalling the cached value if it is called upon by functions at a 
## later time.

## makeCacheMatrix creates an object which contains four functions. These
## functions are passed to the parent environment using the '<<-' assignment
## operator.
##    - set() initializes x as a function argument and inv as a NULL variable in
##      the parent environment. This clears any previously cached values of inv.
##    - get() retrieves the cached value of x 
##    - setinv() sets the inputs arguments for inv to the parent environment.
##    - getinv() retrieves the cached value of inv

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function(){
    x
  }
  
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


## cacheSolve uses the output from makeCacheMatrix to generate an inverse matrix.
## It initializes the inverse matrix by calling the cached value. If there is a
## cached matrix then the user is notified and it is passed. Otherwise, the function
## then retrieves the cached matrix that needs to be inverted and solves it. This
## newly solved matrix is cached back to the makeCacheMatrix object and returned
## to the user. 

cacheSolve <- function(x, ...) {
  
  inv <- x$getinv()
  
  if(!is.null(inv)){
    message('Retrieving cached matrix.')
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
