## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function makes a special matrix. It defines 4 functions(set,getvariable,
##  setinverse, getinverse) and returns them as a list.
## set : set the values in cache
## getvariable: returns the matrix 
## setinverse: save the inverse in cache
## getinverse: returns the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  temp <- NULL
  set <- function(y){
    x <<- y
    temp <<- NULL
  }
  getvariable <- function() x
  setinverse <- function(inverse) temp <<- inverse
  getinverse <- function() temp
  list(set = set, getvariable = getvariable, setinverse = setinverse
       , getinverse = getinverse)
}


## Write a short comment describing this function
## This function returns the inverse of the matrix. It first looks for it in 
## the cache. If cache is not null then it returns the inverse else it calculates
## the inverse using 'solve()' function and saves it in the cache.

cacheSolve <- function(x, ...) {
  temp <- x$getinverse()
  if(!is.null(temp)){
    message("getting cache data")
    return(temp)
  }
  data <- x$getvariable()
  temp <- solve(data)
  x$setinverse(temp)
  temp
        ## Return a matrix that is the inverse of 'x'
}
