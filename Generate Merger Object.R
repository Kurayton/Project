

getMergerObject <- function(file, ticks) {
  
#--- Names ---#
  
  #--- Get info from file name ---#
  merger.Name <- strsplit(substring(file, 10, nchar(file) - 4), "-")
  T.Symbol <- merger.Name[[1]][1]
  A.Symbol <- merger.Name[[1]][2]
  
  #--- Find the merger index in Ticks.csv ---#
  merger.Index <- intersect(which(T.Symbol == ticks$Target.Ticker), which(A.Symbol == ticks$Acquirer.Symbol))
  if(length(merger.Index) != 1) {warning(paste("Merger Index is not unique! ", T.Symbol, A.Symbol, sep="-"))}
  #FIX ME: could add some logic here to get the right one with the date or something
  
  #--- Get Target & Acquirer information ---#
  Target <- c(T.Symbol, as.character(ticks$Target.Name[merger.Index]), as.character(ticks$Target.Industry[merger.Index]))
  Acquirer <- c(A.Symbol, as.character(ticks$Acquirer.Name[merger.Index]), as.character(ticks$Acquirer.Industry[merger.Index]))
  Names <- data.frame(Target, Acquirer, stringsAsFactors=F)
  rownames(Names) <- c("Symbol", "Name", "Industry")
  
#--- Dates ---#
  
  #--- Get Merger Announcement Date (MAD) in format 'YYYY-MM-DD' ---#
  MAD <- as.Date(paste(substring(file,1,4),"-",substring(file,5,6),"-",substring(file,7,8), sep=""))
  dateSDC <- MAD
  
  #--- If MAD falls on a Saturday or Sunday it won't be found in the Dates array ---#
  if(weekdays(MAD) == "Sunday") {MAD <- MAD+1}
  if(weekdays(MAD) == "Saturday") {MAD <- MAD-1}
  if(dateSDC != MAD) {warning(paste("MAD not found in excel header! ", T.Symbol, A.Symbol, sep="-"))}

  Dates <- c(as.Date(ticks$Original.Date.Announced[merger.Index]), MAD, dateSDC)
  Dates <- data.frame(Dates)
  rownames(Dates) <- c("Original MAD", "MAD", "SDC Date")
  #effectiveDate = MED,
  #FIX ME
              #--- Get Merger Effective Date (MED) in format 'YYYY-MM-DD' ---#
#               MED <- as.character(ticks$Date.Effective[merger.Index])
#               if(nchar(MED) != 0) {
#                 MED <- as.Date(MED)
#               } else {MED <- as.Date("1970-01-01")}

  
  #--- Read Merger Table ---#
  mergerTable <- read.csv(paste(indir, file, sep="/"), header=TRUE)
        
        RTI <- apply(mergerTable[,seq(2, dim(mergerTable)[2],3)], 2, getRTI)
        timeLine <- as.Date(substring(names(mergerTable[ ,seq(2, dim(mergerTable)[2], 3)]), 4), format="%Y.%m.%d")        
		    announcementIndex <- which(MAD == timeLine)
        dummy <- ifelse(timeLine >= MAD, 1, 0)
        cutDataFrame <- cleanArray(data.frame(timeLine, RTI, dummy))
		    before <- which(cutDataFrame$dummy == 0)
        after <- which(cutDataFrame$dummy == 1)

  #--- Create Merger List ---#
  mergerlist <- list(
                 fileName = substring(file, 0, nchar(file) - 4),
                 Names = Names, 
                 Dates = Dates,
                 Rumour = ticks$Began.as.Rumour[merger.Index], 
                 reliabilityMeasures = c(mean(colSums(mergerTable[,seq(2, announcementIndex,3)]), na.rm=T), 
                                         mean(colSums(mergerTable[116:540,seq(2, announcementIndex, 3)]), na.rm=T), 
                                         mean(colSums(mergerTable[328, seq(2, announcementIndex, 3)]), na.rm=T)),
                 dataFrameRTI = cutDataFrame,
                 summaryStatsRTI = getStats(cutDataFrame, before, after)
                 #Beta=(getBeta(data.frame(Dates,RTI), MAD)))
  )
  return(mergerlist)
}