## makeCacheMrix - Function takes a matrix as an argument and creates a  cache of the inverse of a matrix 

## This function creates a special "matrix" object that can cache its inverse.
## The function also creates 3 internal helper functions 
## the setMatrix() - sets the Matrix and scopes it to the global env
## the getMatrix() - retrives the matrix setMatrix to the glbMartixVar and returns it
## the setInverseMatrix() function sets  the inverse of the matrix 
## the getInverseMatrix() function returns the inverse of the matrix stored in lclInvMatrixVar

makeCacheMatrix <- function(glbMatrixVar = matrix()) {
  
  lclInvMatrixVar <- NULL
  setMatrix <- function(argMatrix) {
    glbMatrixVar <<- argMatrix
    lclInvMatrixVar <<- NULL
  }
  getMatrix <- function() glbMatrixVar
  setInverseMatrix <- function(inverseMatrix) lclInvMatrixVar <<- inverseMatrix
  getInverseMatrix <- function() lclInvMatrixVar
  list(setMatrix=setMatrix, getMatrix=getMatrix, setInverseMatrix=setInverseMatrix, getInverseMatrix=getInverseMatrix)
  
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.  #
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  
  invMatrixVar <- x$getInverseMatrix()            ## get the matrix from the makeCachematrix - getInverseMatrix()
  if(!is.null(invMatrixVar)) {              ## check to see if the matrix is cached and return it
    message("getting cached data....")
    return(invMatrixVar)
  }
  
  ## Matrix is not cached , so create it and return it
  tmpMatrixData <- x$getMatrix()
  
  ## Using tryCatch to gracefully catch errors and warnings
  ## In some cases the matrix sent in could be a singularity
  invMatrixVar <-tryCatch ( {   
    solve(tmpMatrixData)      ## solve() returns the inverse of the matrix 
  } , warning = function(catchWarning) {
    message(" Matrix Invers returned with warning: ",catchWarning)
    return
  } , error = function(catchERR){
    message(" Matrix Inverse failed:",catchERR)
    return
  })
  
  ## Everthing good with inverse - return the inversed matrix

  x$setInverseMatrix(invMatrixVar)
  invMatrixVar
  
}
