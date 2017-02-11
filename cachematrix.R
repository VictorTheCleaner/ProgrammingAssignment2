## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix creates an invertible matrix for use with cacheSolve
## function assumes that the matrix passed into it is 'sane', that is square and invertible
## this function is akin to the makeVector function

makeCacheMatrix <- function(x = matrix()) {

		# take m and set to NULL
		m <- NULL
		
		#uses set before changing anything
		
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
		
		#uses get before changing anything
        get <- function() x
		
		#now set the inverse
        setinverse <- function(inverse) m <<- inverse
		#get it again after change
        getinverse <- function() m
		#make a list
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function
## Returns a matrix that is the inverse of x, given that it is invertible and square

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
		 m <- x$getinverse()
		 #If already done, return it
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
		
		#else get the matrix
        data <- x$get()
		
		#solve matrix for inverse
        m <- solve(data, ...)
		# save result
        x$setinverse(m)
		#return result
        m
}
