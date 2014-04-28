## These functions are used together to cache the inverse of a
#  given invertible matrix. makeCacheMatrix takes a given matrix, stores it and 
#  includes a getter/setter for its inverse if it has already been calculated.
#  cacheSolve will take a makeCacheMatrix object, return already calculated inverse if
#  one exists; otherwise calculates, stores, and returns the inverse.   

##Sample Usage
# m <- matrix(1:4, 2,2) #Creates invertable matrix
# matrixToSolve <- makeCacheMatrix(m) #Initializes makeCacheMatrix object using m 
# cacheSolve(matrixToSolve) #First call solves and stores the inverse of the matrix
# cacheSolve(matrixToSolve) #Second call retireves pre-caluculation, prints message and returns inverse

## makeCacheMatrix is a special matrix object that stores a matrix, and calculates, stores, and 
#  retrieves its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x 
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve checks to see if the inverse of makeCacheMatrix has 
#  already been computed. If so, it retieves and returns  the already
# computed inverse. If not, it computes, caches, and returns the inverse.

cacheSolve <- function(x, ...) {
        
  m <- x$getinverse()   # retrieve pre-calculated inverse if one exists
  if(!is.null(m)) {     # if existing inverse was retrieved, print message and return matrix 
    message("getting cached data")
    return(m)
  }
  data <- x$get()       # if no inverse was retrieved, get matrix to calculate
  m <- solve(data, ...) # use the solve function to create the inverse
  x$setinverse(m)       # store the calculation in the object for future use
  m                     # Returns a matrix that is the inverse of 'x'
}
