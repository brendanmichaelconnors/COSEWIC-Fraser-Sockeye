Panel1 <- function(data1) {
  #Returns a plot for panel 1
  
  #import raw data and subset for DU
  dataRaw1 <- read.delim(file="data/AllDU.panel1.data.txt", header=T)
  data1 <- cbind(dataRaw1$Year, dataRaw1$enroute.loss, dataRaw1$catch, dataRaw1$escape, dataRaw1$exploit, dataRaw1$DUID) #matrix of escapement, canadian harvest and US harvest - years are columns
  colnames(data1) <- c("Year", "Enroute.Loss", "Catch", "Escapement", "Exploitation.Rate","DUID")
  head(data1)
  
  #Subset data1 to include only the rows corresponding to the current working DUID identified in the panel.combine R document
  #If the DUID does not exist, will produce a blank plot

  data1<-data1[data1[,6] == g, ]
  
  if (nrow(data1)==0) {
    
    plot(1:10, 1:10, type = "n", xlab = " ", ylab = " ", axes = FALSE)
    title("No Corresponding Data for this DU", cex.main=0.8, line = -10)
 
  } else {
  
  #create matrix of Escapement, Catch, Enroute Loss
  data1LCE <- rbind(data1[,4], data1[,3], data1[,2])
  colnames(data1LCE) <- data1[,1]
  
  #Add years with NAs so that data starts at 1950
  missYrs <- data1[1,1] - 1950
  addData <- matrix(data=NA, nrow=3, ncol=missYrs)
  missYrs2add <- seq(from=1950, to=data1[1,1]-1, by=1)
  colnames(addData) <- missYrs2add
  data1plot <- cbind(addData, data1LCE)
  
  #create matrix of Exploitation Rate
  addDataER <- matrix(NA, missYrs, ncol(data1))
  data1ER <- rbind(addDataER, data1)
  data1ER[1:length(missYrs2add),1] <- missYrs2add
  
  #determine largest value for y-axis
  data1summed <- data1[,2]+data1[,3]+data1[,4]

  #determine the height of the yaxis based on largest bar in graph
  most <- ((max(data1summed, na.rm=T))/1000)
  most <- most+(0.3*most)
  least <- (min(data1summed, na.rm=T))/1000
  top <- signif(most, digits=1)
  if(top < most){
    top <- top + 500
  }

  Years <- c(1950,1970,1990,2010)
  
  # a function to create pretty axis breaks for the variable y axis, used in plotting
  yy <- labeling::extended(least, most, 4, only.loose=TRUE) 
  
  #plot Enroute Loss, Catch, and Escapement
  par(mar=c(3,2.2,2.5,1.5), bty="c")
  barplot(data1plot/1000, col=c(grey(0.25),grey(0.75),grey(1)), space=0, axisnames=F, ylim=c(0,most), axes=F)
  axis(1, at=seq(0,60,20), labels=Years, line=0, cex.axis=1, padj=-0.75, tck=-0.015)
  axis(2, at=yy, line=-0.7, cex.axis=1, padj=1, tck=-0.015)
  mtext("Number of fish (000s)", side=2, line=0.7, cex=0.75)
  mtext("Year", side=1, line=1.5, cex=0.75)
  
  #plot Exploitation Rate on 2nd axis
  par(mar=c(3,2.2,2.5,1.5),new=TRUE)
  plot(data1ER[,1], data1ER[,5], col="red", axes=F, xlab="", ylab="", ylim=c(0,1), lwd=2, type="l", yaxs="i")
  axis(4, at=c(0,0.33,0.66,1), labels=c("0","33","66","100"), line=-0.7, cex.axis=1, padj=-1, tck=-0.015,col.axis="red",col="red")	
  mtext("Exploitation rate (%)", side=4, line=0.5, cex=0.75,col="red")
  
  #add legend to plot
  par(mar=c(0,0,0,0),new=TRUE) 
  x<-0:1;y<-0:1;plot(x,y,xaxt="n",yaxt="n",type="n",bty="n")
  legend(x=0.125, y=1, legend=c("Escapement","Catch","En-route loss"), fill=c(grey(0.25),grey(0.75),grey(1)), 
         border=c("black","black","black"), bty="n", cex=0.85, ncol=1, seg.len=0.65)
  
  #add plot label
  mtext("(a)", side=3, line=-1.5, adj=0.05, cex=1)
 
  #end of ifelse loop
   }
  
#end of function
}

