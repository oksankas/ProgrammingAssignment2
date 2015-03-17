## Below are two functions that could be used to create a specail object that stores a square matrix and 
## caches its inverse

## The first function, MakeCacheMatrix creates a special "matrix" list containing a function to
## 1) set the value of the matrix, 2) get the value of the matrix,
## 3) set the value og the inverse, 4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
			Inv <- NULL
			set <- function(y) {
					x <<- y
					Inv <<- NULL
			}
			get <- function() x
			setinv <- function(solve) Inv <<- solve
			getinv <- function() Inv
			list(set = set, get = get,
			setinv = setinv,
			getinv = getinv)
}


## The second function calculate the inverse of the specail "matrix" created with the above function.
##First it checks whether the inverse has been already calculated and gets it from the cache if "yes".
## Otherwise, it calculates the inverse of the data and caches its value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        Inv <- x$getinv()
        if(!is.null(Inv)) {
        		message("getting cached data")
        		return(Inv)
        }
        data <- x$get()
        Inv <- solve(data, ...)
        x$setinv(Inv)
        Inv
}
