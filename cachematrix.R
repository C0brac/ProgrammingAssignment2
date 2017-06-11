## -------------------------->Homework Week 4

## cache the inverse of matix
#made a matrix in order to test
A <- matrix( c(5, 1, 0,
               3,-1, 2,
               4, 0,-1), nrow=3, byrow=TRUE)

#this Function will assign and cache the matrix
makeCacheMatrix <- function(x = matrix()) {
   p<- NULL
   set<- function(r) {
      x<<- r
      p<<- NULL
   }
   get <- function() x
   setInverse<- function(inverse) p<<- inverse 
   getInverse<- function() p
   list (set = set, get= get,
         setInverse= setInverse,
         getInverse= getInverse)
}

## Here we Inverse
cacheSolve <- function(x, ...) {
        p<- x$getInverse()
        #if it's not 'null' print out the matriX
        if (!is.null(p)) {
           message("Looks like we got it here...")
           return(p)
        }
        #Else, here we go making it inversed
        data<- x$get()
        p<- solve(data, ...) 
        x$setInverse(p)
        return(p)
}
