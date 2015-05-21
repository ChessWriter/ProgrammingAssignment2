## Function to Cache the Inverse of a Matrix
## The function uses solve() to calculate inverse
##  a provided matrix and caches its results using the scoping rules in R

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL

        ### 1. Sets the value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }

        ### 2. Obtains the value of the matrix
        get <- function() x

     	### 3. Sets the value of the inverse of the matrix
        setsolve <- function(solve) m <<- solve

	### 4. Obtains the value of the inverse of the matrix
        getsolve <- function() m

	# return our list
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## From a given makeCacheMatrix object, returns the inverse 
##   of the matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve() # retrieve from cache

        if(!is.null(m)) { # we got something...
                message("getting cached data")
                return(m)
        }

	# cache isn't filled yet
        data <- x$get()       # get it
        m <- solve(data, ...) # solve it
        x$setsolve(m)	      # set it
        m		      # return it
}