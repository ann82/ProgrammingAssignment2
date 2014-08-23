# Program to compute the inverse of a matrix and cache it in order to avoid repeated computation. The following two 
# functions help in caching and calculating the inverse

# makeCacheMatrix creates a list to perform
# 1. Set the matrix value
# 2. Get the matrix
# 3. Set the inverse matrix value
# 4. Get the inverse matrix value

# Assumption is made that the matrix is always invertible.

makeCacheMatrix <-function(x=matrix)
{
        # Setting the variable matx to NULL
        matx<-NULL           
        
        # Setting the matrix value
        setmatx<-function(y)
        {
                x<<-y        
                matx<<-NULL                       
        }
        
        # Getting the matrix
        getmatx<-function()x
        
        # Settng the inverse of the matrix
        setinvmatx<-function(inv) matx <<-inv    
        
        # Getting the inverse of the matrix
        getinvmatx<-function() matx              
        
        # List containing the functions
        list(setmatx=setmatx,getmatx=getmatx,
             setinvmatx=setinvmatx,getinvmatx=getinvmatx)
}

# cacheSolve returns the inverse of the matrix. It checks if the inverse has been computed earlier, if so it returns 
# that value else it computes the inverse and caches the value using the setinvmatx()

cacheSolve <-function(x,...)
{
        # Gets the inverse of the matrix
        matx<-x$getinvmatx()   
        
        # Checks if the inverse has been computed earlier.
        if(!is.null(matx)){                     
                
                message("Get the cached data.")
                return(matx)
        }
        
        # Gets the matrix and sets it the data variable
        data<-x$getmatx()   
        
        # The inverse of the matrix happens here
        matx<-solve(data,...)    
        
        # Caches the inverse matrix value
        x$setinvmatx(matx) 
        
        # Final result
        matx
}
