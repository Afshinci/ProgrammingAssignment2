## This R script contains two functions,the first one stores a Matrix
## and its inverse and the second one calculates the inverse matrix

## This function creates a list that can store a matrix(set), return 
## the stored matrix(get), store the inverse of aforementioned 
## matrix(setinverse), and return the inverse matrix (getinverse)


makeCacheMatrix <- function(x = matrix()) {
	invrs<-NULL
	set<-function(y){
		x<<-y
		invrs<<-NULL
	}
	get<-function() x
	setinverse<-function(inverse) invrs<<-inverse
	getinverse<-function() invrs
	list(set=set,get=get,
	     setinverse=setinverse,
	     getinverse=getinverse)	
}


## This function takes the list created by the previous function
## and calculates the inverse of stored matrix and puts it in the list
## again. If the inverse is already calculated, it will not calculate it again

cacheSolve <- function(x, ...) {
       
	invrs<-x$getinverse()
	if(!is.null(invrs)){
		message("getting cached matrix")
		return(invrs)
	}
	matrx<-x$get()
	invrs<-solve(matrx)
	x$setinverse(invrs)
	invrs	
}
