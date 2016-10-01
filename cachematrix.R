## This file contains functions that manipulate a
## matrix wrapper object capable of caching a matrix's
## inverse value.

## creates a cacheMatrix object as follows:
## cacheMatrix#get() gets the value of the matrix
## cacheMatrix#get.inverse() gets the value of the matrix's inverse
## cacheMatrix#set(new.m), set the value of the matrix to m
## cacheMatrix#set.inverse(new.i) sets the value of the matrix's inverse to i

makeCacheMatrix <- function(x = matrix()) {
  # inverse is initially null
  inverse <- NULL
  
  # get the matrix
  get <- function() x
  
  # set the inverse
  get.inverse <- function() inverse

  # set the matrix
  set <- function(new.x) {
    x <<- new.x
    inverse <<- NULL
  }
  
  # set the inverse
  set.inverse <- function(new.i) {
    inverse <<- new.i
  }
  
  # return value should be an object with a
  # list of functions to:
  # get the matrix value, 
  # set the matrix value,
  # get the cached inverse
  # set the cached inverse
  list(
    get = get,
    set = set,
    get.inverse = get.inverse,
    set.inverse = set.inverse
  )
}

## From the assignment:
## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  cached.inverse = x$get.inverse()
  if(is.null(cached.inverse)) {
    cached.inverse <- solve(x$get(), ...)
    x$set.inverse(cached.inverse)
  }
  cached.inverse
}

## some happy path testing ---

## for any given matrix m,
## a cacheMatrix cm created using makeCacheMatrix(m)
## with the inverse computed and cached with cacheSolve(cm)
## cm.get.inverse() should equal solve(m)
test.CacheSolve <- function(m) {
  cm <- makeCacheMatrix(m)
  
  # make sure the inverse is being cached
  solved.inverse = solve(cm$get())
  cacheSolve(cm) # mutability makes me itch, but this is the assignment.
  if(!all(cm$get.inverse() == solved.inverse)) 
    warning("cm$get.inverse() not set/solved by cacheSolve()")
  
  # make sure the cache is getting used
  new.i <- matrix(1:4, nrow=2, ncol=2)
  cm$set.inverse(new.i)
  if(!all(cm$get.inverse() == new.i))
    warning("cacheSolve() not using cm$get.inverse()")
}

## for a given matrix M,
## when makeCacheMatrix is called with m
## an object sould be created with two fields:
## matrix and inverse. when matrix is set, it
## nullifies inverse.
test.MakeCacheMatrix <- function(m) {
  cm <- makeCacheMatrix(m)
  
  # test gets
  if (is.null(cm$get()))         warning("cm$get() is null")
  # test nullification of inverse
  if (!is.null(cm$get.inverse()))  warning("cm$get.inverse() is NOT null")

  # test set of inverse
  cm$set.inverse(matrix(2:5, nrow=2, ncol=2))
  if (is.null(cm$get.inverse())) warning("cm$set.inverse() didn't replace value")
  
  # test set of matrix and renullification
  new.m <- matrix(1:4, nrow=2, ncol=2)
  cm$set(new.m)
  if (!all(cm$get() == new.m))    warning("cm$set(new.m) didn't replace value")
  if (!is.null(cm$get.inverse())) warning("cm$set.inverse() didn't nullify old value")
}
