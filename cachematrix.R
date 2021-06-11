## There are two functions here, makeCacheMatrix and cacheSolve
## 
## makeCacheMatrix is a factory function to create an R object which we can
## then save its current matrix value and its inverse matrix value
## 
## cacheSolve is a function to calculate the inverse of a matrix based on the 
## created object using makeCacheMatrix if the matrix inverse value is not null
## then it will be returned, else it will save the calculated inverse value
## before returning it which will be the case initially

## makeCacheMatrix() will create an object consisting a matrix and its inverse 
## value to call this function and save it into a variable, we can use:
##
## > a <- matrix(c(2,3,4,1), nrow = 2, ncol = 2)
## > matrixA <- makeCacheMatrix(a)

makeCacheMatrix <- function(matrixValue = matrix()) {
    inverseValue <- NULL
    set <- function(matrixSetValue) {
        matrixValue <<- matrixSetValue
        inverseValue <<- NULL
    }
    get <- function() matrixValue
    setInverse <- function(inverseData) inverseValue <<- inverseData
    getInverse <- function() inverseValue
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve() will return the inverse value of a matrix by passing the 
## previously created object into the argument, eventually this function will
## return the inverse value of the object's matrix, to use this funciton,
## we can use the created matrixA object as follows:
##
## > cacheSolve(matrixA) ## which will print
## [,1] [,2]
## [1,] -0.1  0.4
## [2,]  0.3 -0.2
##
## After initially call cacheSolve, everytime we call cacheSolve after that:
## > cacheSolve(matrixA)
## getting cached data
## [,1] [,2]
## [1,] -0.1  0.4
## [2,]  0.3 -0.2

cacheSolve <- function(matrixObject, ...) {
    cachedInverseValue <- matrixObject$getInverse()
    if(!is.null(cachedInverseValue)) {
        message("getting cached data")
        return(cachedInverseValue)
    }
    data <- matrixObject$get()
    inverseValue <- solve(data, ...)
    matrixObject$setInverse(inverseValue)
    inverseValue
}
