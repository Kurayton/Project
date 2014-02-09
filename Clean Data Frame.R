

#--- Get rid of values = 0 for RTI/BSI/SBI ---#
cleanArray <- function(array) {

  if(is.data.frame(array)) {
    zeros <- which(array[,2] == 0)
    array <- array[-zeros,]
    return(array)
  }
  else {
    zeros <- which(array == 0)
    array <- array[-zeros]
    return(array)
  }
}