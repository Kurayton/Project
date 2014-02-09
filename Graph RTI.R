

#--- Plot merger's Daily Indicators ---#

graphRTI <- function(list) {
  
  mergerNames <- paste(list[['Names']]$Target[2], list[['Names']]$Acquirer[2], sep=" - ")
  charDateSDC <- as.character(as.Date(list[['Dates']][3,1]))
  Rumour <- as.character(list[['Rumour']])
  Title <- paste(mergerNames, "\n", 
                 "Announcement Date: ", charDateSDC, "\n", 
                 "Rumoured Deal: ", Rumour)
  
  MAD <- as.numeric(as.Date(list[['Dates']][2,1]))
  dataFrame <- list[['dataFrameRTI']]
  fileName <- paste(list[['fileName']], ".png", sep="")
  
  RTI.graph <- ggplot(dataFrame,aes(x=timeLine,y=RTI))+
    geom_point(colour="black")+
    stat_smooth(method="loess", colour="black")+
    labs(title=Title)+
    xlab("Time")+
    ylab("RTI")+
    geom_vline(xintercept=MAD, colour="red", linetype = 2, size=0.9)+
    theme_stata()
  ggsave(paste(outdir.RTI, fileName, sep="/"), width=20, height=10)

  print(paste(fileName, "Plotted", sep=": "))
}