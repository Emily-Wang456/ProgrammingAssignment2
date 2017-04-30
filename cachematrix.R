## This function creates a special "matrix" object that can cache its inverse.
## If the input matrix is not a square matrix, this function prints
##"Not a square Matrix"

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        if(ncol(x)!=nrow(x)){
                print("Not a square Matrix")
        }
        set <- function(y) {
                if(ncol(y)!=nrow(y)){
                        print("Not a square Matrix")
                }
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated (and the 
##matrix has not changed), then the cachesolve should retrieve the inverse from
##the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        length<-ncol(data)
##Creat a identity matrix of the same size as the input matrix
        a<-rep(c(rep(0,length),1),length-1)
        b<-matrix(c(1,a),length,length)
        i <- solve(data,b, ...)
        x$setinverse(i)
        i
}
