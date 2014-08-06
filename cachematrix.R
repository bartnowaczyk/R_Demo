
## makeCacheMatrix combine 4 features - set and get of the original matrix, set and get cached matrix that is inverese of original matrix (x)

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        ## put original function to cache and NULL to cached inverse matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## returns original matrix from cache
        get <- function() x
        ## set received inversed matrix to cache 
        setInverse <- function(inv) m <<- inv
        ## returns cached inversed matrix
        getInverse <- function() m
        ## returns dataset with four functions available
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}



## cacheSolve function checks if there is a cached matrix that is inverse  of x. 
## If true it returns this matrix, if false it going to inverse x, write it to the cache and return it.
cacheSolve <- function(x, ...) {
	## check if there is any cached inversed matrix
  	m <- x$getInverse()

   	if(!is.null(m)){
  		message("Getting cached matrix");
  		return(m)
  	}	
  	## get original matrix
  	data <-x$get()
  	## compute an inverse matrix
  	m<-solve(data,...)
  	## write an inversed matrix to the cache
  	x$setInverse(m)
 	m 	
}
