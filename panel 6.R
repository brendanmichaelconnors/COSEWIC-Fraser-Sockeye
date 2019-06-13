Panel6 <- function(data6) {
  #Returns a plot for panel 6

  #make data5 from data4 (which is imported from sourcing panel 4)
  dataRaw4 <- read.csv("data/Fr SK EFS data for B.Conners_LongForm.UPDATE.csv")
  data6<-dataRaw4[which(dataRaw4$DUID == g),]
  
  if (nrow(data6)==0) {
    
    plot(1:10, 1:10, type = "n", xlab = " ", ylab = " ", axes = FALSE)
    title("No Corresponding Data for this DU", cex.main=0.8, line = -10)
    
  } else {
    
    #inner if else loop
    if(all(is.na(data6$EFS))==TRUE){
      
      plot(1:10, 1:10, type = "n", xlab = " ", ylab = " ", axes = FALSE)
      title("No Corresponding EFS Data for this DU", cex.main=0.8, line = -10)
      
    } else {  
  
  #put EFS in log space
  data6log <- log(data6[4])
  
  #generate smoothed EFS = 4yr running average (same as Gant et al. 2011 -- using 1yr before and 2yrs after)
  data6SS <- rollapply(data6log, 4, mean)
  
  #remove first yr and last 2yrs from Year (x) & smoothed EFS (SS) because cannot have 4yr average for those years
  M3 <- nrow(data6)-2
  Year <- data6[2:M3,3]
  SS <- data6SS[,1]
  SS12 <- SS[(length(Year)-12):(length(Year))]

  runif(1) # this is because some PCs need the seed to be initialized
  
  #run model using all yrs (model is in panel 4, which is sourced above)
  bay.linear= jags(data = list("x"= Year,"y"= SS,"N"= length(SS)), inits = , parameters.to.save= c("a","b"), 
                   model.file="linear.escape.bug", n.chains = 1, n.burnin = 5000, n.thin = 5, n.iter = 100000, DIC = TRUE)
  out<-as.mcmc(bay.linear)
  
   raw.change.medianALL <- quantile(((exp(out[[1]][,2]*(length(SS12-1)-1)))-1)*100,probs=0.5)

   #determine plot limits
    
    x_top6<-max((as.numeric(exp(out[[1]][,2]*(length(SS12-1)-1)))-1)*100)
    x_bottom6<-min((as.numeric(exp(out[[1]][,2]*(length(SS12-1)-1)))-1)*100)
    x_length6<-length((as.numeric(exp(out[[1]][,2]*(length(SS12-1)-1)))-1)*100)
    
    xx <- labeling::extended(x_bottom6, x_top6, 10, only.loose=TRUE) 
    xx <- unique(round(xx, digits = 0))
    
     #breaks
    fixedbreaks_6 <- seq(x_bottom6,x_top6,by =((x_top6- x_bottom6)/20))
    
    peak <- max(hist((as.numeric(exp(out[[1]][,2]*(length(SS12-1)-1)))-1)*100, plot=F, breaks = fixedbreaks_6)$counts)
    top6 <- signif(peak, digits=1)
    if(top6 < peak){
      top6 <- peak + 1000
    }


  #plot estimated rate of change as historgram
  par(mar=c(3,2.2,2.5,1))
  histData <- hist((as.numeric(exp(out[[1]][,2]*(length(SS12-1)-1)))-1)*100, axes=F, xlab="", ylab="", main="", ylim=c(0,top6),xlim=c(x_bottom6,x_top6), breaks = fixedbreaks_6, col=grey(0.95))
  ablineclip(v= raw.change.medianALL,y1=0,y2= top6,lwd=2,lty=1,col="dark grey")
  axis(1, at = xx, line=-0.45, cex.axis=1, padj=-0.75, tck=-0.015)
  #axis(2, at=seq(0,2000,4000), line=-0.75, cex.axis=1, padj=1, tck=-0.015)
  mtext("% change over 3 gen. (all years)", side=1, line=1, cex=0.75)
  #mtext("Frequency", side=2, line=0.75, cex=0.75)
  
  #add plot label
  mtext("(f)", side=3, line=1, adj=-0.06, cex=1)

  }

  #end of outer if  else loop
}
#end of function
}


