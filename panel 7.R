Panel7 <- function(data7) {
  #Returns a plot for panel 7

  #import raw data and subset for DU
  dataRaw7 <- read.csv("data/Fraser Sockeye (SR and Juvenile Data Dec 2014).csv")

  #get 'Sockdat July 11 2014 (COSEWIC).csv' for dataRaw3
  dataRaw3 <- read.csv("data/Sockdat July 11 2014 (COSEWIC).csv")

  #subset for DU

  data7a<-dataRaw7[which(dataRaw7$DUID == g),]

  
  if (nrow(data7a)==0) {
    
    plot(1:10, 1:10, type = "n", xlab = " ", ylab = " ", axes = FALSE)
    title(main = "No Corresponding Data for this DU", cex.main=0.8, line = -10)
  
  } else {
  
  data7b <- dataRaw3[which(dataRaw3$DUID == g),]
  
    #inner if else loop
    if(all(is.na(data7b$effective.female.spawners))==TRUE){
      
      plot(1:10, 1:10, type = "n", xlab = " ", ylab = " ", axes = FALSE)
      title("No Corresponding EFS Data for this DU", cex.main=0.8, line = -10)
      
    } else {  


  data7 <- cbind(data7a, data7b$effective.female.spawners)


  #calculate freshwater survial
  data7$FS <- data7[,4]/data7[,5]
  
  #generate smoothed Juv/EFS = 4yr running average (using 1yr before and 2yrs after)
  SFS <- rollapply(data7[,6], 4, mean)

  #remove first yr and last 2yrs from Year because cannot have 4yr average for those years
   M3 <- nrow(data7)-2
   Year <- data7[2:M3,2]
   Sdata7 <- cbind(data7$Brood.Year[2:64], SFS)

  #alter data so starts at 1950 and has every year
  allDates <- seq(1950, 2013, 1)
  data7AD <- matrix(data=allDates, nrow=length(allDates), ncol=1)
  colnames(data7AD) <- "Year"
  colnames(Sdata7) <- c("Year","SFS")
  data7full <- merge(data7AD, Sdata7, by="Year", all.x=T)
  #data7full <- cbind(allDates, SFS[3:66])
    
  #determine top of y-axis
  data7NNA <- subset(data7full, data7full[,2] != 'NA')
  most7 <- max(data7NNA[,2])
  least7 <- min(data7NNA[,2])
  top7 <- signif(most7, digits=2)
  if(top7 < most7){
    top7 <- top7 + 40
  }
  
  # a function to create pretty axis breaks for the variable y axis, used in plotting
  yy <- labeling::extended(least7, top7, 6, only.loose=TRUE) 
  
  #par(mar=c(2,2.4,2,0.3))
  plot(data7full, type="l", axes=F, xlab="", ylab="", ylim=c(0,top7+(0.1*top7)), lwd=2, yaxs="i")
  points(data7full,pch=16,cex=0.5)
  axis(1, at=seq(1950,2013,20), line=0, cex.axis=0.75, padj=-1.75, tck=-0.015)
  axis(2, at=yy, line=-0.47, cex.axis=0.75, padj=1.5, tck=-0.015)
  mtext("Smoothed freshwater survival\n (smolt/EFS)", side=2, line=0.6, cex=0.75)
  mtext("Brood year", side=1, line=0.85, cex=0.75)
  
  #add plot label
  mtext("(a)", side=3, line=1, adj=-0.15, cex=1)

  #end of inner loop
  }
  
  #end of outer loop
  }
#end of function
}

