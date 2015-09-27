## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#makeCacheMatrix creates a matrix A and enable to get/set the matrix and get/set its inverse
#cacheSolve computes the inverse of the matrix A; if the inverse already exists, the function gets the inverse from the cache

makeCacheMatrix <- function(A = matrix()) {
	inv <- NULL
	set <- function(B) {
                A <<- B
               	#assuming that we set the matrix to a different matrix
                inv <<- NULL
        }
        get <- function() A
        setinverse <- function(inverse) inv <<- inverse 
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(A, ...) {
        ## Return a matrix that is the inverse of 'A'
    inv <- A$getinverse()
	if(!is.null(inv)) {
		message("Getting cache inverse")
		return(inv)
	}
	data <- A$get()
	inv <- solve(data)
	A$setinverse(inv)
	inv

}
