## Put comments here that give an overall description of what your 
## functions do 
 
## Write a short comment describing this function 
 
 
 makeCacheMatrix <- function(x = matrix()) { 
#		print(x)
# we initialize the m as null
		m <- NULL
# set funtion just return cached value		
		set <-function(y)
		{x <<-y
		m <<- NULL	
		}
# get just gets data received		
		get <-function() x
# this function cached the inverse value value		
		setInver <-function(solve) m<<-solve
# this allows us to know if we already cached the inverse		
        getInver <-function() m
# list of funtion available		
 		list(set = set,get = get,setInver = setInver
		, getInver = getInver	)
 
 } 
 
 
 
 
## Write a short comment describing this function 
 
  cacheSolve <- function(x, ...) { 
         ## Return a matrix that is the inverse of 'x'
		m<-x$getInver()
		## if we already have cached it we send a message and
		## returns value
            if (!is.null(m)) {
		    message("getting cached data")
		    return(m)
            }
		## if not we get the data and obtains the inverse	
		data <-x$get()
            m <- solve(data,...)
		## we cached the value	
		x$setInver(m)
            m      		 
 } 
