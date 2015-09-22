makeCacheMatrix <- function(x = matrix()) {
      ## x is a square invertible matrix 
      ## the function returns list of functions:
      ##    set the matrix (in cache)
      ##    get the matrix (from cache)
      ##    set the inversed matrix (in cache)
      ##    get the inverse (from cache)
      ## This funmction is very similar to the "mean" function in the example
      ## The argument "slv" - stands for Solving the matrix
      slv <- NULL
      set <- function(y) {
            x <<- y
            slv <<- NULL
      }
      get = function() x
      setslv = function(solve) slv <<- solve 
      getslv = function() slv
      list(set=set, get=get, setslv=setslv, getslv=getslv)
}

cacheSolve <- function(x, ...) {
      ## where x is the output of makeCacheMatrix()
      ## the function returns the the inverted matrix of x
      
      ## first the function searches the cach for the solotion
      slv <- x$getslv()
      ## if there is a solution in the cache (not the first time calculation)
      ## it will retreve the solution from cached along with a message
      if (!is.null(slv)){
            message("getting cached data")
            return(slv)
      }
      
      ## if there is NO solution in the cache (first time calculation)
      ## it will solve the matrix 
      Omatrix <- x$get()
      slv <- solve(Omatrix, ...)
      
      # last it will save the solution in the cahche
      x$setslv(slv)
      # and print it to the console
      return(slv)
}


## you can try my code usinf this example
##    r = runif(10000000)
##    matrix = matrix(r, nrow=1000, ncol=1000)
##    test = makeCacheMatrix(matrix)
##    cacheSolve(test)

