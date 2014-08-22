## makeCacheMatrix creates the matrix that can cache its inverse
makeCacheMatrix <-function(x=matrix)
{
        ## Setting the matrix to NULL
        matx<-NULL           
        
        ## Setting the value
        setmatx<-function(y)
        {
                x<<-y        
                matx<<-NULL                       
        }
        
        ## Getting the matrix
        getmatx<-function()x
        
        ## Settng the inverse of the matrix
        setinvmatx<-function(inv) matx <<-inv    
        
        ## Getting the inverse of the matrix
        getinvmatx<-function() matx              
        
        list(setmatx=setmatx,getmatx=getmatx,
             setinvmatx=setinvmatx,getinvmatx=getinvmatx)
}

## cacheSolve performs the inverse of the matrix
cacheSolve <-function(x,...)
{
        ## Gets the inverse of the matrix
        matx<-x$getinvmatx()   
        
        ## Checks if the inverse were calculated anytime.
        if(!is.null(matx)){                     
                
                message("Get the cached data.")
                return(matx)
        }
        
        ## Gets the matrix and sets it the data variable
        data<-x$getmatx()   
        
        ## The inverse of the matrix happens here
        matx<-solve(data,...)    
        
        ## Sets the inverse matrix value
        x$setinvmatx(matx) 
        
        # Final result
        matx
}