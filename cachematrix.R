## Matrix inversion being an expensive time consuming operation, can be optimzed by cache the results
## We shall write a special function makeCacheMatrix that helps to cache a "matrix" and its "inverse".
## cacheSolve would be the other method that would be looking up in the above cache before going for the inversion.

## makeCacheMatrix helps to cache the value of a matrix and its inverse
## in lexical scope. It also ensures that the cache is cleared when the value of the matrix changes.
makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  
  get <- function() x
  
  set <- function(newMatrix){
    # Reset the matrix
    x <<- newMatrix
    # Reset the Inverse as well
    inverseMatrix <<- NULL
  }
  
  getInverse <- function() inverseMatrix
  
  setInverse <- function(invMat) inverseMatrix <<- invMat
  
  # Expose the possible operations on the cache
  list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)
}


## cacheSolve function attempts to first search for the inverse of the matrix 
## in the cache and if not found, then calculates the inverse and ensures that the calculated value 
## is stored in cache for future use.

cacheSolve <- function(x, ...) {
  invMat <- x$getInverse()
  if(! is.null(invMat)) {
    message("Inverse Fetched from Cache")
    return(invMat)
  }
  
  inputMat <- x$get()
  inverseMat <- solve(inputMat)
  
  # Cache the newly calculated inverse
  x$setInverse(inverseMat)
  ## Return a matrix that is the inverse of 'x'
  inverseMat
}
