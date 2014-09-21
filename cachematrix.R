## Put comments here that give an overall description of what your
## functions do

## The two functions are used to create a special object that stores a matrix 
## and caches its inverse matrix.


## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its inverse,
## which is a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

## This function calculates the inverse matrix of the special "matrix"
## created by the above function. It first checks to see if the inverse
## has already been calculated. If so, it gets the inverse from the cache 
## and skips the computation. Otherwise, it calculates the inverse matrix
## and sets the value of the inverse in the cache via the setInverse function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv
}
