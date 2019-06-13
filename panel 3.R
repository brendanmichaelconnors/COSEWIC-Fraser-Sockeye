Panel3 <- function(data3) {
  #Returns a plot for panel 3

  #import raw data and subset for DU
  dataRaw3 <- read.csv("data/Sockdat July 11 2014 (COSEWIC).csv")
  head(dataRaw3)
  
  #Subset data1 to include only the rows corresponding to the current working DUID identified in the panel.combine R document
  #If the DUID does not exist, will produce a blank plot
  data3<-dataRaw3[which(dataRaw3$DUID == g),]
  
  #outer if else loop
  if (nrow(data3)==0) {
    
    plot(1:10, 1:10, type = "n", xlab = " ", ylab = " ", axes = FALSE)
    title("No Corresponding Data for this DU", cex.main=0.8, line = -10)
    
  } else {
  
  #calculate log(recruits / EFS)
  data3$recruits <- data3$rec3 + data3$rec4 + data3$rec5
  data3$RperEFS <- data3$recruits / data3$effective.female.spawners
  data3$log <- log(data3$RperEFS)
  
  #inner if else loop
  if(all(is.na(data3$effective.female.spawners))==TRUE){
   
    plot(1:10, 1:10, type = "n", xlab = " ", ylab = " ", axes = FALSE)
    title("No Corresponding EFS Data for this DU", cex.main=0.8, line = -10)
      

  } else {
  
  #alter data so starts at 1950 and has every year
  allDates <- seq(1950, 2013, 1)
  data3AD <- matrix(data=allDates, nrow=length(allDates), ncol=1)
  colnames(data3AD) <- "Brood.Year"
  data3full <- merge(data3AD, data3, by="Brood.Year", all.x=T)
  
  
  #create dataframe with log(recruits / EFS)
  data3plot <- cbind(data3full$Brood.Year, data3full$log)
  
  #determine max value for yaxis
  most <- max(data3plot[,2][!is.na(data3plot[,2])])
  top3 <- signif(most, digits=1)
  if(top3 < most){
    top3 <- top3 + 1
  }
  Years <- c(1950,1970,1990,2010)
    
  #plot
  par(mar=c(3,2.2,2.5,1))
  plot(data3plot, type="l", axes=F, xlab="", ylab="", ylim=c(-2,top3), lwd=2, yaxs="i")  
  axis(1, at=seq(1950,2013,20), line=0, cex.axis=1, padj=-0.75, tck=-0.015)
  axis(2, at=seq(-2,top3,1), line=-0.7, cex.axis=1, padj=1, tck=-0.015)
  mtext(expression(paste("Recruits per spawner (log"[e]," scale)")), side=2, line=0.7, cex=0.75)
  mtext("Brood year", side=1, line=1.5, cex=0.75)

  #add plot label
  mtext("(c)", side=3, line=1, adj=-0.06, cex=1)
  
  #end of inner if else loop
  }
  
  #end of outer if else loop
  }
  #end of function
}

