## The goal is to develop a pair of functions which
## implements the caching of an inverse of a matrix


## The function makeCacheMatrix returns a list consisting of four
## functions that encode a matrix (object) that caches
## its own inverse. The four functions are as follows:
#
# (1) set: store new matrix
# (2) get: get stored matrix
# (3) comp_inv: compute & store inverse
# (4) get_inv: get inverse

makeCacheMatrix <- function(x = matrix()) {
            inv <- NULL
            set <- function(y)
            {
              if ( !identical(x, y))
              {
                x <<- y
                inv <<- NULL
                message("I have updated the matrix and reset the inverse!")
              }
              else
              {
                message("No need to update matrix/inverse (you provided the same matrix).")
              }
            }
          get <- function() x
          comp_inv <- function() inv <<- solve(x)
          get_inv <- function() inv
          list(set = set, get = get, comp_inv= comp_inv, get_inv=get_inv)
}


## cacheSolves takes as input a matrix x in the
## encoding determined by makeCacheMatrix and returns the inverse of x.
## If the inverse is not already cached, it first computes the inverse.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        if (! is.null(x$get_inv()))
        {
          message("Returning the cached inverse!")
          return(x$get_inv())
        }
        ## Otherwise compute inverse
        message("Computing inverse first.")
        x$comp_inv()
        x$get_inv()
}
