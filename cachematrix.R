## makeCacheMatrix function creates a list containing a function:
## 1. sets value of a matrix
## 2. gets the value of the matrix
## 3. Setes the value of the inverse of the matrix
## 4. Gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve function returns the inverse of the matrix defined in the previous function
## first it checks if the inverse has been calculated within the cache
## If it has, then it skips the computation and returns the value using get
## If not, then it computes the inverse and stores it in the cache using setinverse

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
    }
      
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

