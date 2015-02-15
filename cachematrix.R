## R Programming Course on coursera
## Programming Assignment 2: Lexical Scoping
##
##	>>>  Extra comments added for peer assesment  <<<

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


## returns the inverse of a matrix
##
##	x	the base matrix (actually a list returned by makeCacheMatrix)
cacheSolve <- function(x, ...) {
	#check that the output from makeCacheMatrix was supplied
	#(simply assume that if x$getinverse exists we have the correct list object)
	if (class(x) == "matrix"){
		message(cat("Please supply the output from makeCacheMatrix",
			"rather than a matrix"))
		return(NULL)
	}
	else if (class(x) != "list" || is.null(x$getinverse)){
		message("Please supply the output from makeCacheMatrix")
		return(NULL)
	}

	#get the cached inverse (will be null if not yet cached)
	imatrix <- x$getinverse()
	if(!is.null(imatrix)) {
		#cached, show message and return the cached value
    	message("getting cached data ..... ")
        return(imatrix)
    }
	#get the matrix
    data <- x$get()
	#invert using solve
    imatrix <- solve(data, ...)
	#cache the result
    x$setinverse(imatrix)
    #return the inverted matrix
	imatrix
}
