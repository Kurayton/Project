

#--- Viewing option set to decimal without exponents ---#
options(scipen=99)
library(ggplot2)
library(ggthemes)

#---  Set up the environment depending on user --#
if( Sys.getenv("username")=="keith" ) {
  indir <- "k:/ticks-data/mergers"
  outdir <- "k:/ticks-data/clayton"
  srcdir <- "k:/ticks-data/R Scripts"
  tckdir <- "k:/ticks-data/Ticks/Ticks.csv"
  outdir.RTI <- paste( outdir, "/RTI Plots", sep="" )
} else {
  indir <- "C:/Project V2/Data"
  outdir <- "C:/Project V2/Returned"
  srcdir <- "C:/ThesisProject"
  tckdir <- "C:/Project V2/Ticks/Ticks.csv"
  outdir.RTI <- paste( outdir, "/RTI Plots", sep="" )
}
#--- Create folders ---#
if( !file.exists(outdir) ) dir.create(outdir)
if( !file.exists(outdir.RTI) ) dir.create(outdir.RTI)

#--- Get dTimes vector (first column of all excel files) ---#
temp <- read.csv(paste(indir, dir(indir)[1], sep="/"), header=TRUE)
dTimesSeconds <- as.numeric(temp[328:655,1])
dTimesMinutes <- dTimesSeconds/(1000*60)

#--- Source R functions ---#
source(paste(srcdir, "Generate Merger Object.R", sep="/"))
source(paste(srcdir, "RTI Calc.R", sep="/"))
source(paste(srcdir, "Clean Data Frame.R", sep="/"))
source(paste(srcdir, "Graph RTI.R", sep="/"))
source(paste(srcdir, "Get RTI Statistics Function.R", sep="/"))
#source(paste(srcdir, "Regression Function.R", sep="/"))

ticks <- read.csv(tckdir, header=TRUE)

#--- ---#
counter <- 0

#--- Start Time 1 ---#
time1 <- Sys.time()
      #--- Generate Merger List ---#
      mergerList <- list()
      mergerFiles <- dir(indir)
      for(file in mergerFiles) {
        name <- substring(file, 10, nchar(file) - 4)
        name <- gsub("-", "", name)
        mergerList[name] <- list(getMergerObject(file, ticks))
        print(name)
		    counter <- counter + 1
      }
#--- Stop Time 1 ---#
endtime1 <- Sys.time()

#--- Print Times ---#
print(paste("It took", as.character(as.numeric(endtime1 - time1)), "seconds to create",  as.character(counter), "Merger Objects."))

#--- Start Time 2 ---#
time2 <- Sys.time()
      #--- Run Analysis ---#
      #graphs <- sapply(mergerList, graphRTI)
#--- Stop Time 2 ---#
endtime2 <- Sys.time()

#--- Print Times ---#
print(paste("It took", as.character(as.numeric(endtime2 - time2)), "seconds to plot RTI's."))
