

## makeCacheMatrix takes the matrix x as an input and creates a list that contains functions to set the matrix, get the matrix,
## set inverse of the matrix and get inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse<-NULL
        set<-function(y){
                x<<-y
                inverse<<-NULL
        }
        get<-function() x
        setinverse<-function(solve) inverse<<-solve
        getinverse<-function() inverse
     
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        

}


## Function cacheSolve computes the inverse of the matrix x from makeCcheMatrix if inverse has not been set already, otherwise
                ##ir returns the cached inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse<-x$getinverse()
                if(!is.null(inverse)){
                        message("getting cashed data")
                        return(inverse)
                }
                data<-x$get()
                inverse<-solve(data, ...)
                x$setinverse(inverse)
        
}
