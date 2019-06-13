Panel8 <- function(data8) {
  #Returns a plot for panel 8
  
  #import raw data and subset for DU
  dataRaw7 <- read.csv("data/Fraser Sockeye (SR and Juvenile Data Dec 2014).csv")
  #subset for DU
  data7a <-dataRaw7[which(dataRaw7$DUID == g),]
  data7b <- dataRaw3[which(dataRaw3$DUID == g),]
  
  if (nrow(data7a)==0) {
    
    plot(1:10, 1:10, type = "n", xlab = " ", ylab = " ", axes = FALSE)
    title("No Corresponding Data for this DU", cex.main=0.8, line = -10)
    
  } else {
    
    recruits <- data7b$rec3 + data7b$rec4 + data7b$rec5
    data8 <- cbind(data7a, recruits)
    
    #calculate marine(&river) survial
    data8$MS <- data8[,5]/data8[,4]
    
    #generate smoothed Rec/Juv = 4yr running average (using 1yr before and 2yrs after)
    SMS <- rollapply(data8[,6], 4, mean)
    #SMS <-data8$MS
    
    #remove first yr and last 2yrs from Year because cannot have 4yr average for those years
     M3 <- nrow(data8)-2
    Year <- data8[2:M3,3]
    Sdata8 <- cbind(Year, SMS)
    
    #alter data so starts at 1950 and has every year
    allDates <- seq(1950, 2013, 1)
    data8AD <- matrix(data=allDates, nrow=length(allDates), ncol=1)
    colnames(data8AD) <- "Year"
    data8full <- merge(data8AD, Sdata8, by="Year", all.x=T)
    #data8full <- cbind(allDates, SMS[3:66])
    
    #determine top of y-axis
    data8NNA <- subset(data8full, data8full[,2] != 'NA')
    most8 <- max(data8NNA[,2])
    least8 <- min(data8NNA[,2])
    top8 <- signif(most8, digits=1)
    if(top8 < most8){
      top8 <- top8 + 0.02
    }
    
    yy <- labeling::extended(least8, top8, 6, only.loose=TRUE) 
    
   # par(mar=c(2,2.4,2,0.3))
    plot(data8full, type="l", axes=F, xlab="", ylab="", ylim=c(0,top8+(0.1*top8)), lwd=2, yaxs="i")
    points(data8full,pch=16,cex=0.5)
    axis(1, at=seq(1950,2013,20), line=0, cex.axis=0.75, padj=-1.75, tck=-0.015)
    axis(2, at=yy, line=-0.47, cex.axis=0.75, padj=1.5, tck=-0.015)
    #axis(2, at=seq(0,1,1), line=-0.47, cex.axis=0.75, padj=1.5, tck=-0.015)
    mtext("Smoothed post-fry survival\n (recruits/smolt)", side=2, line=0.5, cex=0.75)
    mtext("Brood year", side=1, line=0.85, cex=0.75)
    
    #add plot label
    mtext("(b)", side=3, line=1, adj=-0.15, cex=1)
    
    
    #end of loop
  }
  
  #end of function
}

