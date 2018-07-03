## Assignment Week 3: Caching the Inverse of a Matr

##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##This function will set and get the matrix and its inverse matrix.

makeCacheMatrix <- function(x = matrix()){
invMatrix <- NULL
setMatrix <- function(y){
  x <<- y
  invMatrix <<- NULL
    }
getMatrix <- function() x
setInverse <- function(inverse) invMatrix <<- inverse
getInverse <- function() invMatrix
list(setMatrix = setMatrix,getMatrix = getMatrix,setInverse = setInverse,getInverse = getInverse)
}

## This function computes/ retrives from cache the inverse of the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) 
{
invMatrix <- x$getInverse()
if(!is.null(invMatrix)) {
message("getting cached data")
return(invMatrix)
}
data <- x$getMatrix()
invMatrix <- solve(data, ...)
x$setInverse(invMatrix)
return(invMatrix)   ## Return a matrix that is the inverse of 'x'
}


##### Testing  ####

###my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))

 my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
 my_matrix$getMatrix()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
 my_matrix$getInverse()
#NULL
 cacheSolve(my_matrix)
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
 cacheSolve(my_matrix)
#getting cached data
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
 my_matrix$getInverse()
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
 my_matrix$setMatrix(matrix(c(6, 7, 8, 9), 2, 2))
 my_matrix$getMatrix()
#[,1] [,2]
#[1,]    6    8
#[2,]    7    9
 my_matrix$getInverse()
#NULL
 cacheSolve(my_matrix)
#[,1] [,2]
#[1,] -4.5    4
#[2,]  3.5   -3
 cacheSolve(my_matrix)
#getting cached data
#[,1] [,2]
#[1,] -4.5    4
#[2,]  3.5   -3
 my_matrix$getInverse()
#[,1] [,2]
#[1,] -4.5    4
#[2,]  3.5   -3
 


