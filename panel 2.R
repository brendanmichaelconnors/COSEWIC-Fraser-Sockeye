Panel2 <- function(data2) {
  #Returns a plot for panel 2
 
  #import raw data and subset for DU
  dataRaw2 <- read.csv("data/skall_update.csv")

  ##Subset data1 to include only the rows corresponding to the current working DUID identified in the panel.combine R document
  #If the DUID does not exist, will produce a blank plot
  data2<-dataRaw2[which(dataRaw2$DUID == g),]
  
  if (nrow(data2)==0) {
    
    plot(1:10, 1:10, type = "n", xlab = " ", ylab = " ", axes = FALSE)
    title("No Corresponding Data for this DU", cex.main=0.8, line = -10)
    
  } else {
  
  #alter data so starts at 1950, has every year, and data from each year are added together for each column
  allDates <- seq(1950, 2015, 1)
  data2full <- matrix(data=NA, nrow=length(allDates), ncol=5)
  colnames(data2full) <- c("Year", "males", "females", "eff_fem", "FPSM")
  data2full[,1] <- allDates
  for(i in 1:nrow(data2full)){
    data2cut <- subset(data2, Year == data2full[i,1])
    if(nrow(data2cut) > 0){
      data2full[i,2] <- sum(data2cut[,8],na.rm=T)
      data2full[i,3] <- sum(data2cut[,9],na.rm=T)
      data2full[i,4] <- sum(data2cut[,11],na.rm=T)
      data2full[i,5] <- sum(data2cut[,12],na.rm=T)

    }
  }
  
 
  #create dataframe with data for stacked barplot
  data2plot <- rbind(data2full[,4], data2full[,2], data2full[,5])
  colnames(data2plot) <- data2full[,1]
  
  #determine the height of the yaxis based on largest bar in graph
  most <- (max(data2plot[1,]+data2plot[2,]+data2plot[3,], na.rm=T))/1000
  most <- most+(0.3*most)
  least <- (min(data2plot[1,]+data2plot[2,]+data2plot[3,], na.rm=T))/1000
  #top2 <- signif(most, digits=1)
  #if(top2 < most){
  #  top2 <- top2 + 0.2*top2
  #}
  
  Years <- c(1950,1970,1990,2010)
  
  # a function to create pretty axis breaks for the variable y axis, used in plotting
  yy <- labeling::extended(least, most, 4, only.loose=TRUE) 

    #plot
  par(mar=c(3,2.2,2.5,1))
  barplot(data2plot/1000, col=c(grey(0.25),grey(0.75),grey(1)), space=0, ylim = c(0, most), axes=F, axisnames=F)  
  axis(1, at=seq(0,60,20), labels=Years, line=0, cex.axis=1, padj=-0.75, tck=-0.015)
  axis(2, at=yy, line=-0.7, cex.axis=1, padj=1, tck=-0.015)
  mtext("Number of fish (000s)", side=2, line=0.7, cex=0.75)
  mtext("Year", side=1, line=1.5, cex=0.75)
  
  par(mar=c(0,0,0,0),new=TRUE) 
  x<-0:1;y<-0:1;plot(x,y,xaxt="n",yaxt="n",type="n",bty="n")
  legend(x=0.125, y=1,  legend=c("EFS", "Males","Female PSM"), fill=c(grey(0.25),grey(0.75),grey(1)), 
         border=c("black","black","black"), bty="n", cex=0.85, ncol=1, seg.len=0.65)
  
  #add plot label
  mtext("(b)", side=3, line=-1.5, adj=0.05, cex=1)

  #end of if else loop
  }
  #end of function
}


