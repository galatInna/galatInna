#makeCacheMatrix creates a special 'matrix' object that can cache its inverse
makeCacheMatrix = function(x=matrix()){
    answer <- NULL #we don't have an answer
    get_original_data <-function() {x} #getting an original data(matrix)
    set_assign_answer <- function(answer) { #when called
        #it sends result up and write/reassign it to answer which was null before
        answer <<- answer 
    }
    get_answer <- function() {answer}#when called, it returns 
    #the cached/saved (in answer) value of result
    list(get_original_data = get_original_data,
         set_assign_answer = set_assign_answer,
         get_answer = get_answer)
}

# this function computes the inverse of the special 'matrix'
#returned by makeCacheMatrix above. If the inverse has already been calculated 
#(and the matrix has not changed), then the cachesolve 
#should retrieve the inverse from the cache.
cacheSolve <- function (w=list,...){
    answer <- w$get_answer()
    if(!is.null(answer)){
        message('getting cached data')
        return(answer)
    }
    initial_data <- w$get_original_data()
    answer <-solve(initial_data)
    w$set_assign_answer(answer)
    answer
}
