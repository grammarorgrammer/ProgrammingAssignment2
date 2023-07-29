rm(list = ls())
.libPaths("C:/Rlibrary")


## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than computing it repeatedly


## This function generate four functions in a list.
## While you put a matrix in a "set" function, the output of "get" will change
## base on the matrix you put in.
makeCacheMatrix <- function(x = matrix()) {
  # set default of inverse matrix as NULL
  inv_ma <- NULL
  
  # set inverse matrix back to NULL while new replacement in
  set_matrix <- function(replace_ma){
    x <<- replace_ma
    inv_ma <<- NULL
  }
  # get matrix in function
  get_matrix <- function() x
  
  # replace inverse matrix from NULL(default) to inverse of inputted matrix
  set_inverse <- function(inverse) inv_ma <<- inverse
  # get the inverse of matrix
  get_inverse <- function() inv_ma
  return(list(set_matrix = set_matrix, get_matrix = get_matrix,
              set_inverse = set_inverse, get_inverse = get_inverse))
}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
  # get inverse matrix of argument
  inv_x <- x$get_inverse()
  # if the inverse matrix is not NULL(default), means we had already computed it
  # once before.
  if(!is.null(inv_x)){
    message('getting cached data...')
    return(inv_x)
  }
  # if the inverse matrix is NULL(default), we should compute the inverse matrix
  # out and cache it
  else{
    message('setting cached data...')
    inv_x <- solve(x$get_matrix(), ...)
    x$set_inverse(inv_x)
    return(inv_x)
  }
}