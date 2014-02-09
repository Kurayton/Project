

#--- Uniform Cumulative Distribution Function (CDF) ---#
T <- dTimesSeconds[length(dTimesSeconds)]
unifCDF <- dTimesSeconds/T

#--- Function to calculate the Related Trading Indicator (RTI) of each day in   ---#
#--- the Merger from the full vector of daily counts of trade time differences: ---#

## This needs to extract more than just RTI... it needs totalCounts in 1 day. I can already compute the reliability measures so that
## it doesn't trouble the user when using generateMergerObject

getRTI <- function(day) {
  
  tempDay <- day[!is.na(day)]
  tempDaySum <- sum(tempDay)
  if(tempDaySum == 0) {
    return(RTI.KS <- 0)
  }
  
  dailyCountSum <- sum(day)
  if(is.na(dailyCountSum)) {
    amount <- length(which(is.na(day)))
    if(amount >= 1) {warning(paste(merger.Name, " - Amount of NA/NAN in column (day) is serious"))}
    if(amount > 5) {warning(paste(merger.Name, " - Amount of NA/NAN in column (day) is critical"))}
    day <- day[!is.na(day)]
    collapsed <- c(day[328], rev(day[1:327]) + day[329:655])
    CDF <- cumsum(collapsed)/sum(collapsed)
    
    return(RTI.KS <- max(CDF-unifCDF))
    
  } else {
    collapsed <- c(day[328], rev(day[1:327]) + day[329:655])
    CDF <- cumsum(collapsed)/sum(collapsed)
    
    return(RTI.KS <- max(CDF-unifCDF))
  }
  #--- Still need to calculate Cramer von Mises (CVM) ---#
}