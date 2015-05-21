
# set the value of the matrix
# get the value of the matrix
# set the value of inverse of the matrix
# get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL # set the inv to NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, 
             setinverse=setinverse, 
             getinverse=getinverse)
}

# get the inverse of the matrix
# if the inverse of the matrix is not null (already exists) return the cached value and display message " getting cached data"

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data) #solve function works only on square matrices
        x$setinverse(inv)
        inv
}

# Example 
# ---Define a square matrix - in this case a 2x2 matrix---
#> x<-matrix(c(1,4,6,8), nrow = 2, ncol = 2) 

#> a=makeCacheMatrix(x)
#> a$get()
#[,1] [,2]
#[1,]    1    6
#[2,]    4    8


#> cacheSolve(a)
#[,1]    [,2]
#[1,] -0.50  0.3750
#[2,]  0.25 -0.0625

#---When we run the cacheSolve the second time, we get the cached data---

#> cacheSolve(a)
#getting cached data.
#[,1]    [,2]
#[1,] -0.50  0.3750
#[2,]  0.25 -0.0625
