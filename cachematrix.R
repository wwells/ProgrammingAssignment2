## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# sets up a cache of a matrix, returns a cache of inverse
# 1)  empty variable inv to store inverse of matrix
# 2)  sets up set function to store inverse
# 3)  sets up get function to return matrix
# 4)  sets up function that stores value of inverse matrix
# 5)  sets up function to return value for inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        print(list(set=set, get=get, setInverse=setInverse, getInverse=getInverse))
    }

}


## Write a short comment describing this function
# returns the inverse of a matrix by cache value or calculating

# 1)  get cached value
# 2)  if value != NULL, return and exit
# 3)  if value == NULL, calculate value
# 4)  store and return caclulated value

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)){
        message("getting cached data")
        return (inv)
    }
    matrix <- x$get()
    inv <- solve(matrix)
    x$setInverse(inv)
    inv
}

