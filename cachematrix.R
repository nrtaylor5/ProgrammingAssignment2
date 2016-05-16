#Matrix inversion can be a costly function, especially with large matrices.
#There is a benefit to caching the inverse of a matrix and storing it to be recalled,
# rather than have to compute it again.
#
#The following two functions are used to cache the inverse of a matrix.
#
#
#makeCacheMatrix creates a list that:
#sets the value of the matrix
#gets the value of the matrix
#sets the value of the inverse
#gets the value of the inverse



makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  get <- function() x
  setminv <- function(inverse) minv <<- inverse
  getminv <- function() minv
  list(set = set, get = get,
       setminv = setminv,
       getminv = getminv)
}

#cacheSolve returns the inverse of the matrix. It first checks to see if
#the inverse of a matrix has already been computed.  If it has, it displays
#"getting cached data", and returns the invers.  If it has not been computed,
#it computes the inverse of the matrix and caches the inverse.



cacheSolve <- function(x, ...) {
  minv <- x$getminv()
  if(!is.null(minv)) {
    message("getting cached data")
    return(minv)
  }
  
  data <- x$get()
  minv <- solve(data, ...)
  x$setminv(minv)
  minv
}

###################################################################################################################
#Sample Run:
#
# z <- matrix(c(1,2,2,1),2,2)
> z
#[,1] [,2]
#[1,]    1    2
#[2,]    2    1
#> z_Cached <- makeCacheMatrix(z)
#> #Display z_Cached:
#> z_Cached
##$set
#function (y) 
#{
# x <<- y
#  minv <<- NULL
#}
#<environment: 0x000000001704ef38>
#  
#  $get
#function () 
#  x
#<environment: 0x000000001704ef38>
#  
#  $setminv
#function (inverse) 
#  minv <<- inverse
#<environment: 0x000000001704ef38>
#  
#  $getminv
#function () 
#  minv
#<environment: 0x000000001704ef38>
#  
#  > z_Cached$get()
#[,1] [,2]
#[1,]    1    2
#[2,]    2    1
#> cacheSolve(z_Cached)
#[,1]       [,2]
#[1,] -0.3333333  0.6666667
#[2,]  0.6666667 -0.3333333
#> cacheSolve(z_Cached)
#getting cached data
#[,1]       [,2]
#[1,] -0.3333333  0.6666667
#[2,]  0.6666667 -0.3333333
#
