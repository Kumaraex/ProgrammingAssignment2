##We have two methods - makeCacheMatrix and cacheSolve

## makeCacheMatrix has a default square matrix. Of course the user can override
# its default square matrix by providing one by using its inner function - set
# The other inner functions are get - to retrieve the default or supplied square matrix
# setInverse function is used to cache the inverse matrix
# The getInverse function returns the cached inverse matrix 

## cacheSolve function utilizes the makeCacheMatrix to cache the square matrix
# It can supply a matrix if it wants to or use makeCacheMatrix's default square matrix
# Also it first checks if the inverse is already present in the cache, if yes then it suffices
# Else it computes the inverse using the solve function and gives it to makeCacheMatrix for caching


#Here we create a special "matrix" object that can cache its inverse (matrix).
#We supply a default 3x3 square (simple) matrix as the functions lone argument
makeCacheMatrix <- function(x = matrix(rbind(c(-1,2,3),c(-5,2,3),c(9,2,-3)), nrow=3, ncol=3)) {
	inverseSqMat <- NULL

	get <- function() x
	set <- function(mat) {
		x <<- mat
		inverseSqMat <<- NULL
	}
	setInverse <- function(argInverseSqMat) {
		inverseSqMat <<- argInverseSqMat
	}
	getInverse <- function() inverseSqMat
	 
	list(get=get, set=set, setInverse=setInverse, getInverse=getInverse)
}

#Here we compute the inverse of the special "matrix"
cacheSolve <- function(makeCacheMatrix, ...) {

	# Get the inverse matrix from the vector function - makeCacheMatrix()
 	inverseMatrix <- makeCacheMatrix$getInverse()
	
	# If we are lucky, we get the cached version. Return it. That's all folks !
	if(!is.null(inverseMatrix)) {
		message("Hurray ! Got the cached version of the matrix...")
		return(inverseMatrix)
	} else {
		# Else we can get the default matrix and compute its inverse
		defaultSqMatrix <- makeCacheMatrix$get()
		# Set the computed inverse to the cache so that we can use it again.
		invertedMatrix <- solve(defaultSqMatrix)
		makeCacheMatrix$setInverse(invertedMatrix)
	}
	#Alternatively instead of using the default matrix in the makeCacheMatrix()
	#we can also create our own regular square matrix and use it to compute
	#the inverse. We should use the makeCacheMatrix$set() function for this.       
}