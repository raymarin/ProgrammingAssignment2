## These pair of functions compute and cache the inverse of a matrix


## This function creates a special matrix object 

makeCacheMatrix <- function(x = matrix()) {
  # Set the inverse matrix to NULL  
  x_star <- NULL # 
  
  # Function to assign a new numeric matrix to an existing
  # special matrix. In this case the inverse matrix is also
  # reset to NULL so that it needs to be recomputed
  set <- function(y) {
    x <<- y
    x_star <<- NULL
  }
    
  # Function to get the original numeric matrix
  get <- function() x
  
  # Function to update x_star to "inverse"
  setinverse <- function(inverse) x_star <<- inverse
  
  # Function to get the matrix stored in x_star
  getinverse <- function() x_star
  
  # List returned by makeCacheMatrix. This is a list of functions applied on the numeric matrix x
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes and returns the inverse of the special "matrix" 
## returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  # Gets the value x_star for the special matrix created out of the numeric matrix
  x_star <- x$getinverse()
  
  # If finds something different from NULL then returns x_star
  if(!is.null(x_star)) {
    message("Getting cached data")
    return(x_star)
  }
  
  # If x_star is NULL then retrieves the numeric matrix
  mat <- x$get()
  
  # Computes the inverse of the matrix
  x_star <- solve(mat, ...)
  
  # Sets the cache inverse matrix of the special matrix to the matrix computed above
  x$setinverse(x_star)
  
  # returns the inverse matrix
  x_star
}

## Example: 

# Create a special 2x2 matrix
l=2
a <- makeCacheMatrix(matrix(1:l^2, l))

# Compute the inverse matrix
cacheSolve(a)

# Modify the actual data inside the expecial matrix
a$set(matrix(l^2:1, l))

# Recompute the inverse matrix
cacheSolve(a)


