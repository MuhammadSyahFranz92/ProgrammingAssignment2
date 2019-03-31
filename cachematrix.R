> makeCacheMatrix <- function(x = matrix()) {               #set the value of the vecto#
+   i <- NULL
+   set <- function(y){
+     x <<- y
+     i <<- NULL
+   }
+   get <- function()x                                      #get the value of the vector#
+   setinverse <- function(inverse) i <<- inverse
+   getinverse <- function() i
+   list (set = set, 
+         get = get,
+         setinverse = setinverse,
+         getinverse = getinverse
+         )
+   
+ }
> 
> 
> cacheSolve <- function(x, ...) {                          #set the value of the mean#
+         ## Return a matrix that is the inverse of 'x'
+   i <- x$getinverse()
+     if(!is.null(i)){
+       message ("getting chaced data")
+       return (i)
+     }
+     data <- x$get()                                       #get the value of the mean
+     i <- solve(data, ...)
+     x$setinverse(i)
+     i
+   
+ }
> 
> #example
> a <- matrix(c(1,2,3,4),2,2)
> a1 <- makeCacheMatrix(a)
> cacheSolve(a1)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> 
> a <- matrix(c(4,5,6,7),2,2)
> a1 <- makeCacheMatrix(a)
> cacheSolve(a1)
     [,1] [,2]
[1,] -3.5    3
[2,]  2.5   -2
