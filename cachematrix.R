
## To create a mtrix inverse and caching the inverse in separate environment


##It creates the list of functions used by cachesolve function(like get or set)
makeCacheMatrix <- function(x = matrix()) {
     
	#initialize to null	
	cache <- NULL

	#matrix creation in present env.
	set <- function(y) {
                x <<- y
                cache <<- NULL
        }

	#get matrix
	get <- function() x

	#inverting matrix 
	setMatrix <- function(inverse) cache <<- inverse
	
	#getting inverted matrix
	getInverse <- function() cache

	#returning list
	list(set = set, get = get,
             setMatrix = setMatrix,
             getInverse = getInverse)
}

##Solves the matrix if it not found in cache in envvironment. 

cacheSolve <- function(x, ...) {
        
	#get the inverse from cache if it is there
	  
	cache <- x$getInverse()

	#if found the inverse in cache it returns
	if (!is.null(cache)) {
                message("cached data")
                return(cache)
        }

	#ifnot found creates the matrix
	matrix <- x$get()

	#inverses the matrix
	cache <- solve(matrix, ...)

	#puts it in cache
	x$setMatrix(cache)

	#return the cache
	return (cache)
}



