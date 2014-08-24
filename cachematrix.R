## These functions cache the inverse of a square invertible matrix
## 

## makeCacheMatrix - This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

	# argument x is a matrix, you can test by setting up a matrix with something like
	# mat <-(1:4,2,2) which will be a square matrix with the values 1 to 4
	# then do something like:  bigMat<- makeCacheMatrix(mat) 
	# which will set up the matrix object and cache its inverse

	# initialize the inverse to NULL
	m <- NULL

        set <- function(y) {
                x <<- y
                m <<- NULL
        }

	# Get is a function that returns x

        get <- function() x
	

	# setmean  takes as an argument **mean** and return m with the same value as mean
	# not needed here could take it out
	setmean <- function(mean) m <<- mean

	# setinverse is a function that gets the inverse of the matrix
	setinverse<-function(solve) m<<-solve

        # Note: getmean is not needed here and could be removed
	# getmean has no arguments and returns the value of m which is null unless it is cached
	getmean <- function() m
	

 	# getinverse has no arguments and returns the value of m which is null unless it is cached
	getinverse <- function() m

	list(set = set, 
		get = get,
		setinverse = setinverse,
             	setmean = setmean,
		getinverse =getinverse,
             	getmean = getmean)
	

}


## function cachesolve checks to see if the result inverse is cached; if not, it computes the inverse and caches it

cacheSolve <- function(x, ...) {
        # 
	# calculates the inverse of the special "matrix" created with the above function. However, it first checks to see if 
	# the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. 
	
	
	# see if the inverse of x has already been calculated and is in cache
        # m <- x$getmean() NOT NEEDED

	m <- x$getinverse()

        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
	
	# if the inverse has NOT already been calculated then get x from another environment and put it in **data**
        data <- x$get()
	

	# calculate the inverse of data
        #m <- mean(data, ...)  NOT NEEDED

	m <- solve(data,...)

        # x$setmean(m) NOT NEEDED

	x$setinverse(m)
        m
}

