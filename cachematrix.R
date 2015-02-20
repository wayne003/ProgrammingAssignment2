## The first function makeCacheMatrix, same as the example in assignment instruction,  creates a special "vector", which is a list containing a function to
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the mean
## 4. get the value of the mean


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function (x = matrix())
{
  s <- NULL
  set <- function (y)
  {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolution <- function(solution) s<<-solution
  getsolution <- function() s
  return(list(set = set, get = get, setsolution = setsolution, getsolution = getsolution))
}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...)
{
  s <- x$getsolution()
  if (!is.null(s))
  {
    message("Getting cached data...")
    return(s)
  }
  data <- x$get()
  s <- solve(data,...)
  x$setsolution(s)
  return(s)
}
