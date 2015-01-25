# These pair of functions are designed to make matrix inversion more computationally efficient. This is achieved by caching the inverse
# and, if the matrix has not changed, the inverse is retrived from the cache rather than being recalculated.

# Function 1: makeCacheMatrix. The input of this function is an invertible matrix. This function creates a special object (a list of 
# functions) that can take a matrix as input and cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL
  set <- function(y) {
    x <<- y
    mat <<- NULL
  }
  get <- function()  x 
  setinv <- function(inverse) mat <<-  inverse 
  getinv <- function() mat 
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# Function 2: cacheSolve. Input: the object to which makeCacheMatrix has been assigned. This function checks whether the inverse of the 
# matrix input in makeCacheMatrix has been calculated and stored in the cache (if the matrix has not changed). If it has, then the 
# inverse is retrived from the cache. If it has not, the inverse is calculated.

cacheSolve <- function(x, ...) {
  mat <- x$getinv()
  if(!is.null(mat)) {
    message("getting cached data")
    return(mat)
  }
  data <- x$get()
  mat <- solve(data, ...)
  x$setinv(mat)
  mat
}

