## Cache Matrix Inverse


## makeCacheMatrix make a list that holds matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
	mat_inverse <- matrix()
    mat_inverse <- NULL
    set <- function(y) {
        x <<- y
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
        message("getting cached data")
        return(mat_inverse)
    }
    data <- x$get()
    mat_inverse <- solve(data)
    x$set_mat_inverse(mat_inverse)
    mat_inverse
}
