#Generating Functions

#function for the coin flipping [0/1]
coinflip <- function(){
  result <- sample(c(1,0),1,T,c(0.5, 0.5))
  return(result)
}