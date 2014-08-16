## Put comments here that give an overall description of what your
## functions do

#makeCacheMatrix makes a list containing a matrix, and methods to get and 
#set the inverse which is cached in the global environment.  cacheSolve takes a return value of above function and returns the inverse of the matrix

## Write a short comment describing this function

#The inverse of matrix gets set so it can be cached.  The set function nulls the previously cached inverse when a new matrix is assigned.

makeCacheMatrix <- function(x = matrix()) {
	invrs <- NULL
	set <- function(y) {
		x <<- y
		invrs <<- NULL
	}
	get <- function() x
	setinverse <- function(inversIn) invrs <<- inversIn
	getinverse <- function() invrs
	list(set=set, get=get, setinverse = setinverse, getinverse=getinverse)

}


## Write a short comment describing this function

# function checks for a cached value of inverse, and stores it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	invrs <- x$getinverse()
	if(!is.null(invrs)) {
		message("getting cached data")
		return(invrs)
	}
	data<-x$get()
	invrs<-solve(data)
	x$setinverse(invrs)
	invrs
}


