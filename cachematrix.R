## computes the inverse of a matrix object if not cached 
## if cached the inverse is retrieved 

## creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv = NULL
        set = function(y) {
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## computes the inverse of the matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv = x$getinv()
        if(!is.null(inv)){
                return(inv)   
        }
        mat.data = x$get()
        inv = solve(mat.data, ...)
        x$setinv(inv)
        return(inv)
}
