makeCacheMatrix<-function(x=matrix()) {
    inv_x=NULL
    set<-function(y){
        x=y
        inv_x=NULL
    } #set the matrix
    get=function()x #gets the matix
    setinv=function(inv) inv_x=-inv #sets the inverse
    getinv=function() inv_x #gets the inverse
    list(set=set,get=get,setinv=setinv,getinv=getinv) #list for cacheSolve
}

cacheSolve=function(x,...){
    inv_x=x$getinv()
    if(!is.null(inv_x)){
        message("Loading Data")
        return(inv_x) #if already calculated skips computation
    } else {
        inv_x=solve(x$get())
        x$setinv(inv_x)
        return(inv_x) #calculates inverse and sets to cache
    }
}