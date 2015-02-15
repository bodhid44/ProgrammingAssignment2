### R Programming Course on coursera
### Programming Assignment 2: Lexical Scoping
###
###	>>>  Extra comments added for peer assesment  <<<
###
### Three functions available:
### makeCacheMatrix - part of the assignment
### cacheSolve - part of the assignment
### example_cachematrix - example of usage that also includes timings
### to indicate that the second call to cacheSolve returns a cached version

## makeCacheMatrix
## Creates a special "matrix" that caches the inverse of the matrix
## (actually a list containing set/get functions for the matrix and inverse)
##
##	x	the base matrix, default value 1x1 matrix with NA value
##		can assume that the supplied matrix is invertible
##
##	The cached inverse must be reset if the matrix is changed
##
##	internal functions returned via a list are
##		set - sets the base matrix and resets the inverse cache to null
##			y	the new base matrix
##		get	- returns the base matrix
##		setinverse - resets the inverse cache
##			newcache	the inverted matrix to cache
##		getinverse - returns the inverted matrix (or NULL if not yet inverted)
##	
## 	internal variable
##		invcache - the cached, inverted matrix (NULL when not yet cached)	
##
##	return value
##		list containing the set, get, setinverse and getinverse functions
##
makeCacheMatrix <- function(x = matrix()) {
  #if supplied parameter is not a matrix, use empty matrix
  #[assignment states that we should assume that the supplied matrix is 
  #invertible, so no further checking]
  if (class(x)!="matrix"){
    x = matrix()
  } 
  invcache <- NULL
  
  # sets the base matrix, and resets the inverse cache to NULL
  set <- function(y){
    #set the base matrix
    x <<- y	
    #reset the inverse cache
    invcache <<- NULL
	}
  
  # returns the base matrix
  get <- function() x 
	
  # sets the inverse cache (which is calculated elsewhere)
  setinverse <- function(invmatrix) invcache <<- invmatrix

  # returns the inverse cache
  getinverse <- function() invcache
  
  #the returned list containing the functions
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve
## returns the inverse of a matrix
##
##	x	the base matrix (actually a list of functions returned by makeCacheMatrix)
cacheSolve <- function(x, ...) {
  #check that the output from makeCacheMatrix was supplied
  #(simply assume that if x$getinverse exists we have the correct list object)
  if (class(x) == "matrix"){
    message("Please supply the output from makeCacheMatrix rather than a matrix")
    return(NULL)
  }
  else if (class(x) != "list" || is.null(x$getinverse)){
    message("Please supply the output from makeCacheMatrix")
    return(NULL)
  }
  
  #get the cached inverse (will be null if not yet cached)
  imatrix <- x$getinverse()
  if(!is.null(imatrix)) {
    #already cached, so show message and return the cached value
    message("getting cached data ..... ")
    return(imatrix)
  }
  #get the matrix
  data <- x$get()
  #invert using solve
  imatrix <- solve(data, ...)
  #cache the result
  x$setinverse(imatrix)
  #return the inverted matrix [could just use imatrix, 
  #but used return to be more explicit - all exits from the function indicated by return]
  return(imatrix)
}

## example_cachematrix
## function to illustrate the usage of the above functions
example_cachematrix <- function(size = 1000){
  original_matrix <- matrix(rnorm(size^2), nrow=size, ncol=size)
  message(cat("Inverting a",size,"x",size,"matrix."))
  message("['elapsed' should indicate the approximate time required]")
  #start stopwatch, then solve
  stm <- proc.time()
  original_matrix_inverse <- solve(original_matrix)
  print(proc.time() - stm)
  message("\nNow running makeCacheMatrix on the matrix.")
  cached_matrix <- makeCacheMatrix(original_matrix)
  message("Initial call to solve on the cached matrix via cacheSolve")
  message("['elapsed' should be approximately be doubled as not yet cached and computation is required]")
  cs1 <- cacheSolve(cached_matrix)
  print(proc.time() - stm)
  message("Second call to solve on the cached matrix via cacheSolve")
  message("[As retrieving from cache, 'elapsed' should hardly increase and 'getting cached data....' will be output]")
  cs2 <- cacheSolve(cached_matrix)
  print(proc.time() - stm)
  message("\nChecks that inverted matrices are identical")
  oi_ident_cs1 <- identical(original_matrix_inverse, cs1)
  oi_ident_cs2 <- identical(original_matrix_inverse, cs2)
  message(cat("orignal matrix inverted identical to first call of cacheSolve:", oi_ident_cs1))
  message(cat("orignal matrix inverted identical to second call of cacheSolve:", oi_ident_cs2))
}
