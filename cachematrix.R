##  makeCacheMatrix accepts an invertible matrix as input, computes the 
##  inverse and caches this outside of the environment for later retrieval
# test data:
# > cm1 <- makeCacheMatrix(matrix(6:9,2,2))
# > cm1$get()
# [,1] [,2]
# [1,]    6    8
# [2,]    7    9
# > cm1$setInverse(matrix(6:9,2,2))
# > cm1$getInverse()
# [,1] [,2]
# [1,] -4.5    4
# [2,]  3.5   -3
# > cm1$set(matrix(4:7,2,2))
# > cm1$setInverse(matrix(4:7,2,2))
# > cm1$getInverse()
# [,1] [,2]
# [1,] -3.5    3
# [2,]  2.5   -2
# > cmt <- cm1$getInverse()
# > is.null(cmt)
# [1] FALSE
# > cmt
# [,1] [,2]
# [1,] -3.5    3
# [2,]  2.5   -2

makeCacheMatrix <- function(x = matrix()) {
    invMat <- NULL
    set <- function(y){
        x <<- y
    }
    get <- function() return(x)
    setInverse <- function(x) invMat <<- solve(x)
    getInverse <- function() return(invMat)
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
    
}

## cacheSolve computes the inverse of a matrix using a cache value if available
# test data:
# > cm2 <- makeCacheMatrix(matrix(10:13,2,2))
# > cacheSolve(cm2)     #first pass computes value
# > cacheSolve(cm2)     #second pass uses cached value created on first pass

cacheSolve <- function(x, ...) {
    ## cacheSolve is passed an object of class makeCacheMatrix
    invMat1 <- x$getInverse()
    if(!is.null(invMat1)){
        return(invMat1)
    }
    ## inverse was not cached.  compute and cache for future use
    ## calling object already has data
    x$setInverse(x$get())
    return(x$getInverse())
    
}
