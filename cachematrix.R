
#############Objective of the program######################
#Here, we write two functions, makecacheMatrix(), and cacheSolve(). In the
# first function, a special matrix is created.Next, using function cacheSolve(),
#the inverse of matrix is computed. It is assumed that the matrix is invertible.

##############Learning outcomes#####################

# This program demonstrates concept of lexical scoping in R. In addition, it
#also exhibits following features in R:(i) usage of <<-, (ii)output of
#a function can be expressed as list of other functions.

#########Goal of the first function###########

#First, we create a matrix object, using the function makeCacheMatrix(). Its
#body consists of four functions; set(),getnew(),setinverse(),and getinverse().

#The set() fn. is used to change the originally assigned matrix.

#The getnew () fn. just displays the matrix entered, hence has no argument.

#The setinverse() fn. is used to set some value for the inverse of original
#matrix, not necessarily the correct inverse. It has to be kept in mind that 
#dimensions of matrix, "x" and "inv" should be same.

#Last, the getinverse() fn. only displays the matrix assigned to "m" in the
#setinverse() fn.

################################################################
makeCacheMatrix<-function(x=matrix()){
  m <- NULL
  set<-function(n){
    
    x<<- n
    m<<- NULL
  }
  getnew <- function(){
    x
  }
  setinverse<-function(inv){
    m<<- inv
  }
  getinverse <- function(){
    m
  }
  
  list(set=set,getnew=getnew,setinverse=setinverse,getinverse=getinverse)
}

#################Describimng the Second function###################

#Once the matrix is created using makeCacheMatrix(), its inverse needs to be 
# computed. Before computing the inverse, we will check value of "m". If the 
#value of "m$ is not NULL, and matrix assigned to "m" is the correct inverse
#(i.e. product of "x" and "m" is  identity matrix), we will retrieve the 
#answer from the first function. # Otherwise, the inverse is computed and 
#stored in I.

cacheSolve<- function(y1){
  
  
  m<- y1$getinverse()
  # print(y1$getnew())
  # print (m)
  c2=dim(m)
  y2=y1$getnew()
  
  if(!is.null(m)){
    A=m%*%y2
    
    if (identical(A,diag(c2[2]))) {
      
      print("Return cached inverse")
      return (m)
    }
  }
  
  data=y1$getnew()
  I=solve(data)
  y1$setinverse(I)
  I
}


