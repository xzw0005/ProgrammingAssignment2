## 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##    			  If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix by calling solve function in R on that matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
            inv = NULL # Initialize an empty matrix
			set = function(y) {
			        x <<- y
					inv <<- NULL
			}
			get = function() x
			setInverse = function(solve) inv <<- solve
			getInverse = function() inv
			list(set = set, get = get,
				 setInverse = setInverse,
				 getInverse = getInverse)
}


## The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse matrix has already been calculated. 
## If so, it gets the inverse matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse matrix of the data and sets the value of the inverse matrix in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
			inv = x$getInverse()
			if (!is.null(inv)) {
					message("getting cached data")
					return(inv)
			}
			data = x$get()
			inv = solve(data, ...)
			x$setInverse(inv)
			inv		## Return a matrix that is the inverse of 'x'
}
