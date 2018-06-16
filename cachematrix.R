## The functions create a special object that 
## store a matrix and caches its inverse

## The following function creates a matrix object 
## that caches the matrix inverse. The function returns a 
## list of functions that do the following tasks: 
## set the matrix, get the matrix, set (or cache) the matrix 
## inverse, and get the matrix inverse
## 

makeCacheMatrix <- function(x = matrix()) {
		mInv <- matrix()
		mInv <- NULL
		set <- function(y){
				x <<- y
				mInv <<- NULL
		}
		get <- function() x
		setsolve <- function(inverse) mInv <<- inverse
		getsolve <- function() mInv
		list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## The following function calculates the inverse of the matrix
## created by the function 'makeCacheMatrix'. It first checks to 
## see if the inverse has already been calculated. If so, it
## gets the value of the inverse from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mInv <-x$getsolve()
        if(!is.null(mInv)) {
                message("getting cached data")
                return(mInv)
        }
        data <- x$get()
        mInv <- solve(data,...)
        x$setsolve(mInv)
        mInv
}
