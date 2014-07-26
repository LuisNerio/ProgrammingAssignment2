## Put comments here that give an overall description of what your
## functions do

######################################################
########This function takes a matrix and it gives 
########you a list of fuction thas does diferent 
########kind of things with the matrix.
#####################################################

makeCacheMatrix <- function(x = matrix()) {
  
      inv<-NULL                        ##"inv" tells you if the ##inverse of  the matrix it has been already calculated or not
      
      
      set_matrix<-function(y)
      {                          ## "set_matrix" it´s a function that changes the matrix that you preaviously give and 
        x<<-y                    ##  tells you that you have not calculate the new inverse yet
        inv<<-NULL
      }
      
      
      get_matrix<-function()
      {                        ##"get_matrix" returns you the matrix that you enter to the "makeCacheMatrix" function
        x
      }
      
      set_inverse<-function(inv_1)
      {
        inv<<-inv_1            ##"set_inverse" it set the inverse of the matrix to some value that you enter to the function
      }
      
      get_inverse<-function()
      {
        inv                       ##it returns you the value of the matrix
      }
      
      list(set_matrix=set_matrix,get_matrix=get_matrix,set_inverse=set_inverse,get_inverse=get_inverse)  

}

##########################################################################################
#####################cacheSolve
##########################################################################################
###############This function takes a list of functions ("x") that it specifically
############### is the "makeCacheMatrix" and it works wiht it´s functions to cache 
############### the inverse i.e. it calculates the inverse if it has not been calculated
###########################################################################################

cacheSolve <- function(x, ...) {
        inv<-x$get_inverse()                  ##it use the get_inverse function of the list of functions of makeCacheMatrix a 
        ##it´s return to inv, it could be an actual value or a null value
        
        if( !is.null(inv) )
        {                                 ##if the inverse is null it means that it has not been calculated
          message("already solved")       ##if is different to null value it tells you that you have already calculate it
          return(inv)                     ## and it's return
        }
        
        matrix<-x$get_matrix()                ##it returns you the matrix that you enter on "makeCacheMatrx"
        
        inv<-solve(matrix)                    ##it gets the inverse of the matrix
        
        x$set_inverse(inv)                    ## the inverse is return to the makeCacheMatrix function 
        
        inv                                   ## The inverse is return
}
