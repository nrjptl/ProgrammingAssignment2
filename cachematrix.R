## Cache Matrix Inverse


## makeCacheMatrix make a list that holds matrix and it's inverse
## It takes a matrix as an argument and store it into the list,
## along with inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	mat_inverse <- matrix()
    mat_inverse <- NULL
    set <- function(y) {
        x <<- y
		## set mat_inverse = NULL when argument matrix is changes.
        mat_inverse <<- NULL
    }
    get <- function() x
    set_mat_inverse <- function(inverse) mat_inverse <<- inverse
    get_mat_inverse <- function() mat_inverse
    list(set = set, get = get,
         set_mat_inverse = set_mat_inverse,
         get_mat_inverse = get_mat_inverse)
}


## cacheSolve either caches the matrix inverse or
## calculate the inverse if the matrix has been changed

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of matrix returned by 'x$get()'
	mat_inverse <- x$get_mat_inverse()
    if(!is.null(mat_inverse)) {
		## Getting cached mat_inverse from the enviroment.
        message("getting cached data")
        return(mat_inverse)
    }
	
	## Calculate the inverse of the matrix when mat_inverse = NULL
    mat_inverse <- solve(x$get())
    x$set_mat_inverse(mat_inverse)
    mat_inverse
}
