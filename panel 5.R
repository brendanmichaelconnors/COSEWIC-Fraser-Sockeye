Panel5 <- function(data5) {
  #Returns a plot for panel 5
  #make data5 from data4 (which is imported from sourcing panel 4)

  dataRaw4 <- read.csv("data/Fr SK EFS data for B.Conners_LongForm.UPDATE.csv")
  data5<-dataRaw4[which(dataRaw4$DUID == g),]

  if (nrow(data5)==0) {
    
    plot(1:10, 1:10, type = "n", xlab = " ", ylab = " ", axes = FALSE)
    title("No Corresponding Data for this DU", cex.main=0.8, line = -10)
    
  } else {
    
    #inner if else loop
    if(all(is.na(data5$EFS))==TRUE){
      
      plot(1:10, 1:10, type = "n", xlab = " ", ylab = " ", axes = FALSE)
      title("No Corresponding EFS Data for this DU", cex.main=0.8, line = -10)
      
    } else {  
    
  #put EFS in log space
  data5log <- log(data5[4])
  
  #generate smoothed EFS = 4yr running average (same as Gant et al. 2011 -- using 1yr before and 2yrs after)
  data5SS <- rollapply(data5log, 4, mean)
  
  #remove first yr and last 2yrs from Year (x) & smoothed EFS (SS) because cannot have 4yr average for those years
  M3 <- nrow(data5)-2
  Year <- data5[2:M3,3]
  SS <- data5SS[,1]
  
  runif(1) # this is because some PCs need the seed to be initialized
  
  #subset data for most recent 13 yrs
  Year12 <- Year[(length(Year)-12):(length(Year))]
  SS12 <- SS[(length(Year)-12):(length(Year))]
  
  #run model (using most recent 12 yrs)
  bay.linear= jags(data = list("x"= Year12,"y"= SS12,"N"= length(SS12)), inits = , parameters.to.save= c("a","b"), 
                   model.file="linear.escape.bug", n.chains = 1, n.burnin = 5000, n.thin = 5, n.iter = 100000, DIC = TRUE)
  out12<-as.mcmc(bay.linear)

  #estimate median (and 2.5th and 97.5th percentile) rate of change in loge space
  raw.change.median12 <- quantile(((exp(out12[[1]][,2]*(length(SS12-1)-1)))-1)*100,probs=0.5)
  
  #x axis
  x_top5<-max((as.numeric(exp(out12[[1]][,2]*(length(SS12-1)-1)))-1)*100)
  x_bottom5<-min((as.numeric(exp(out12[[1]][,2]*(length(SS12-1)-1)))-1)*100)
  x_length5<-length((as.numeric(exp(out12[[1]][,2]*(length(SS12-1)-1)))-1)*100)
 
  #breaks
  fixedbreaks_5 <- seq(x_bottom5,x_top5,by =((x_top5- x_bottom5)/20))
  
  xx <- labeling::extended(x_bottom5, x_top5, 10, only.loose=TRUE) 
  xx <- unique(round(xx, digits = 0))
  
  #y axis
  peak <- max(hist((as.numeric(exp(out12[[1]][,2]*(length(SS12-1)-1)))-1)*100, plot=F, breaks = fixedbreaks_5)$counts)
  top5 <- signif(peak, digits=1)
  if(top5 < peak){
    top5 <- peak + 1000
  }
  
  
  #plot estimated rate of change as historgram
  par(mar=c(3,2.2,2.5,1))
  histData <- hist((as.numeric(exp(out12[[1]][,2]*(length(SS12-1)-1)))-1)*100, axes=F, xlab="", ylab="", main="", ylim=c(0,top5),xlim=c(x_bottom5,x_top5), breaks = fixedbreaks_5, col=grey(0.95))
  ablineclip(v= raw.change.median12,y1=0,y2= top5,lwd=2,lty=1,col="blue")
  axis(1, at = xx, line=-0.45, cex.axis=1, padj=-0.75, tck=-0.015)
  #axis(2, at=seq(0,2000,4000), line=-0.75, cex.axis=1, padj=1, tck=-0.015)
  mtext("% change over 3 gen. (last 3 gen.)", side=1, line=1, cex=0.75)
  #mtext("Frequency", side=2, line=0.75, cex=0.75)
  
  #add plot label
  mtext("(e)", side=3, line=1, adj=-0.06, cex=1)
  
  
  
  #end of inner if  else loop
    }
    #end of outer if  else loop
  }
  #end of function
}

