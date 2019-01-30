## Develop a matrix object and cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set <- function(y){
x <<- y
inv <<- NULL
  }
get <- function() x
setInverse <- function(solveMatrix) inv <<- solveMatrix
getInverse <- function() inv
list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Calculate the inverse of the matrix
cacheSolve <- function(x, ...) {
## matrix is x's inverse
inv <- x$getInverse()
if(!is.null(inv)){
message("retrieving cached data")
return(inv)
}
data <- x$get()
inv <- solve(data)
x$setInverse(inv)
inv      
}

##I found this testing method by googling a means to test this out. 


test = function(mat){
  ## @mat: an invertible matrix
  
  temp = makeCacheMatrix(mat)
  
  start.time = Sys.time()
  cacheSolve(temp)
  dur = Sys.time() - start.time
  print(dur)
  
  start.time = Sys.time()
  cacheSolve(temp)
  dur = Sys.time() - start.time
  print(dur)
}

set.seed(1110201)
r = rnorm(1000000)
mat1 = matrix(r, nrow=1000, ncol=1000)
test(mat1)
