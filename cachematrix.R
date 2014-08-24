#This function creates a special "matrix" object that can cache its inverse. 

#The first function, makeCacheMatrix has a number of functions within that
#1.sets the value of the object
#2.gets the value of the object
#3.sets the value of the inverse and 
#4.gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

#The second function, cacheSolve calculates the inverse of the special "matrix" 
#object created with the above function. However, it first checks to see if the 
#inverse has already been calculated. If so, it gets the inverse from the cache
#and skips the computation. Otherwise, it calculates the inverse of the data 
#and sets the value of the inverse in the cache via the setinverse function.
#'Solve' function used to calculate the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}

# Below script to test function.

# Matrix Vector
#z <- matrix(-4:-1,nrow = 2, byrow = TRUE)

# makeVector result
#k <- makeCacheMatrix(z)

# cachemean result
#cacheSolve(k)

