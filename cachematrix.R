##The caching used here can also be seen as example of Singleton Design Pattern for
## which inverse is very similar to object which can be instantiated only once when it is null.
## The get() is used to return present value of our current object, while the getinverse
## returns current value of inverse.

## makeInverse function creates a special matrix in accordance with the example shown
## inv is the variable that stores the inverse of the matrix we use solve () function
## to calculate inverse.

makeInverse <- function(x = matrix())
{
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  } 
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse     ##setinverse() is used to set current value of inverse
  getinverse <- function() inv 
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheInverse function as described above is similar to singleton design pattern
## We use get method of special matrix to get current value of the matrix and use solve()
## method to return current value of the inverse of matrix.If inverse has already been calculated we
## return current value of inverse with "inverse cached" message else we return new value of inverse

cacheInverse <- function(x,...) {
  inv <- x$getinverse()
  
  #in the following if block we check if inverse already exists or not
  if(!is.null(inv)){
    message("inverse cached")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  return(inv)
}

##Example
#k <- matrix(1:4,2,2)
#k1 <- makeInverse(x = k)
  ##cacheInverse(k1)

#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5


  ##cacheInverse(k1)
#inverse cached
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

