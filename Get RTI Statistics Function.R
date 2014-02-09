

getStats <- function(dataFrame, before, after) {

    RTI <- dataFrame[,2]
    
    lengths <- c(length(RTI[before]), length(RTI[after]), length(RTI))
    means <- c(mean(RTI[before]), mean(RTI[after]), mean(RTI))
    stdevs <- c(sd(RTI[before]), sd(RTI[after]), sd(RTI))
    maxs <- c(max(RTI[before]), max(RTI[after]), max(RTI))
    
    summaryStatsDataFrame <- data.frame(lengths, means, stdevs, maxs)
    rownames(summaryStatsDataFrame) <- c("Before", "After", "Total")
    return(summaryStatsDataFrame)
}