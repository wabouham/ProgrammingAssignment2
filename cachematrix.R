## The first function makeCacheMatrix() takes an invertible square matrix as
## argument and retuns a special object that can passed as argument to the
## second function cacheSolve(), in order to compute the inverse of that
## matrix, and store its inverse value in cache.
## Sample usage:
## -------------
## myMatrix <- matrix(c(1, 2, 3, 4), nrow=2, ncol=2)
## cacheMatrix <- makeCacheMatrix(myMatrix)
## ## First call to cacheSolve(cacheMatrix) returns computed matrix inverse:
## myComputedInverseMatrix <- cacheSolve(cacheMatrix) 
## ## Second and subsequent calls to cacheSolve(cacheMatrix) returns cached matrix inverse:
## myCachedInverseMatrix <- cacheSolve(cacheMatrix) 

## This function creates a special "matrix" object that can cache its inverse
## It returns a list that is used as the input to the cacheSolve() function 
makeCacheMatrix <- function(x = matrix()) {
    ## Argument:
    ## - x: is a square invertible matrix
    ## Returns: a list containing 4 functions:
    ##   1. set:  sets the matrix
    ##   2. get:  gets the matrix
    ##   3. setInverse:  sets the matrix inverse
    ##   4. getInverse:  gets the matrix inverse
    
    # initialize the variable
    cachedInverse <- NULL

    # define the 4 functions to return in list
    set <- function(y) {
        # use `<<-` to assign a value to an object in an environment 
        # different from the current environment. 
        x <<- y
        cachedInverse <<- NULL
    }

    get = function() x

    setInverse = function(newInverse) cachedInverse <<- newInverse 
        
    getInverse = function() cachedInverse

    # return list of 4 functions
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and
## the matrix has not changed), then cacheSolve should retrieve the inverse
## from cache, otherwise it will calcualte it with solve() function and cache
## its value for future reference
cacheSolve <- function(x, ...) {
    ## Arguments:
    ## - x:  makeCacheMatrix object, which is output of makeCacheMatrix(x)
    ## Returns: a matrix that is the inverse of 'x'

    # get the cached inverse of matrix x
    matrixInverse = x$getInverse()
        
    # if the inverse has already been calculated and cached
    if (!is.null(matrixInverse)){
        # get it from cache and skip the computation 
        message("Getting cached data for matrix inverse")
        # returned the cached data and exit this function
        return(matrixInverse)
    }
        
    # Otherwise, inverse has not been previously calculated and cached, 
    # we need to calculate it 
    # Get the matrix data
    matrixData = x$get()

    # calculate its inverse with solve() function
    matrixInverse = solve(matrixData, ...)
 
    # set the calculated inverse value in cache, using setInverse function
    x$setInverse(matrixInverse)
    
    # return the calcualted matrix inverse value    
    return(matrixInverse)
}
