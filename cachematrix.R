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
cacheSolve <- function (w=list,...){  #function takes a list 
    answer <- w$get_answer() # go to makecacheMatrix and check if there is any results available
    if(!is.null(answer)){ #if we have answer (not NULL)
        message('getting cached data') #print a text showing 'getting...'
        return(answer) # and showing answer
    }
    initial_data <- w$get_original_data() # if we don't have answer yet
    answer <-solve(initial_data) # go to makeCacheMatrix function and ask solve function 
    # to solve a result on initial data
    w$set_assign_answer(answer) # write/set a result to answer using makeCacheMatrix
    answer #print the result
}
