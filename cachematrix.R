## Following two functions can be used to create a matrix that remembers its
## inverse, so it is not needed to calculate the inverse every time it is 
## needed

## creates a list of functions - getters and setters for the matrix and its
## inverse
makeCacheMatrix <- function(x = matrix()) {
    mInverse <- NULL
    
    setMatrix <- function(y){
        x <<- y
        mInverse <<- NULL
    }
    
    getMatrix <- function(){
        x
    }
    
    setInverse <- function(inverse){
        mInverse <<- inverse
    }
    
    getInverse <- function(){
        mInverse
    }
    
    list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, 
         getInverse = getInverse)
}


## returns the stored inverse of the special matrix created using 
## makeCacheMatrix; if there is no inverse stored, computes it, stores it and 
## returns it
cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)){
        message("Retrieving cached inverse ...")
        return(inverse)
    }
    message("Calculating inverse ...")
    matrix <- x$getMatrix()
    inverse = solve(matrix, ...)
    x$setInverse(inverse)
    inverse
}
