## This assignment is all about getting the inverse of a matrix by catching
## the result within a lexical scope of a function.

## The function below creates a unique and new environment.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function (y){
	 x <<- y
	 inv <<- NULL
}
	get <- function () x

	setinverse <- function(inverse) inv <<- inverse	
	getinverse <- function()inv
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}

## The function 'cacheSolve' allows returning the inverse of matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if(!is.null(inv)) {
	 message("getting cached data")
	 return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
}
}
