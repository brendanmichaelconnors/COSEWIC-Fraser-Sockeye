Panel4 <- function(data4) {
  #Returns a plot for panel 4
  
  #import raw data and subset for DU
  dataRaw4 <- read.csv("data/Fr SK EFS data for B.Conners_LongForm.UPDATE.csv")
  data4<-dataRaw4[which(dataRaw4$DUID == g),]
  
  if (nrow(data4)==0) {
    
    
    plot(1:10, 1:10, type = "n", xlab = " ", ylab = " ", axes = FALSE)
    title("No Corresponding Data for this DU", cex.main=0.8, line = -10)
    
  } else {
  
    #inner if else loop
    if(all(is.na(data4$EFS))==TRUE){
      
      plot(1:10, 1:10, type = "n", xlab = " ", ylab = " ", axes = FALSE)
      title("No Corresponding EFS Data for this DU", cex.main=0.8, line = -10)
      
    } else {  
  
  #put EFS in log space
  data4log <- log(data4[4])

  #generate smoothed EFS = 4yr running average (same as Gant et al. 2011 -- using 1yr before and 2yrs after)
  data4SS <- rollapply(data4log, 4, mean)
  
  #remove first yr and last 2yrs from Year because cannot have 4yr average for those years
  M3 <- nrow(data4)-2
  Year <- data4[2:M3,3] +2
  SS <- data4SS[,1]
  
  runif(1) # this is because some PCs need the seed to be initialized
  
  #this specifies the model in BUGS language
  model = paste("
	
					model {
							for (i in 1:N){
							y[i] ~ dnorm(y.hat[i], tau)
							y.hat[i] <- a + b * x[i]
						}
						a ~ dnorm(0, .000001)
							b ~ dnorm(0, .000001)
						tau <- pow(sigma, -2)
						sigma ~ dunif(0, 1000)
					}

			")
  cat(model, file = "linear.escape.bug")# this just saves the model as a BUGS file that will be called by JAGS in next step
  
  
  #center data so as to make model fitting easier
  sc.yr<-as.numeric(scale(Year))
  
  #run model (using all yrs)
  bay.linear= jags(data = list("x"= sc.yr,"y"= SS,"N"= length(SS)), inits = , parameters.to.save= c("a","b"), 
                   model.file="linear.escape.bug", n.chains = 1, n.burnin = 5000, n.thin = 5, n.iter = 100000, DIC = TRUE)
  out<-as.mcmc(bay.linear)
  
  #estimate median rate of change and median y-intercept in loge space
  change.median <- quantile(out[[1]][,2],probs=0.5)
  int.median <- quantile(out[[1]][,1],probs=0.5)
  
  #predict smoothed spawner abundnce based on modelled relationship
  pred.smooth.spw <- int.median + sc.yr* change.median
  TTT <- SS
  TTT[TTT > 0] <- 1
  pred.smooth.spw <- pred.smooth.spw* TTT
  #subset data for most recent 13 yrs
  Year12 <- Year[(length(Year)-12):(length(Year))]
  SS12 <- SS[(length(Year)-12):(length(Year))]
  
  #center data so as to make model fitting easier
  sc.yr12<-as.numeric(scale(Year12))
  
  #run model (using most recent 12 yrs)
  bay.linear= jags(data = list("x"= sc.yr12,"y"= SS12,"N"= length(SS12)), inits = , parameters.to.save= c("a","b"), 
                   model.file="linear.escape.bug", n.chains = 1, n.burnin = 5000, n.thin = 5, n.iter = 100000, DIC = TRUE)
  out12<-as.mcmc(bay.linear)
  
  #estimate median rate of change and median y-intercept in loge space
  change.median12 <- quantile(out12[[1]][,2],probs=0.5)
  int.median12 <- quantile(out12[[1]][,1],probs=0.5)
  
  #predict smoothed spawner abundnce based on modelled relationship
  pred.smooth.spw12 <- int.median12 + sc.yr12* change.median12
  
  #arrange data for plotting
  data4plot <- cbind(Year, SS)
  data4plotNNA <- subset(data4plot, SS != 'NA')
  top4 <- max(data4plotNNA[,2]+2)
  bottom <- min(data4plotNNA[,2]-1)
  minYr <- min(Year)
  maxYr <- max(Year)
  
  # a function to create pretty axis breaks for the variable y axis, used in plotting
  yy <- labeling::extended(bottom, top4, 10, only.loose=TRUE) 
  yy <- unique(round(yy, digits = 0))
  

  #plot smoothed spawners (log scale)
  par(mar=c(3,2.2,2.5,1))
  plot(data4plot, type="l", axes=F, xlab="", ylab="", ylim=c(bottom,top4), lwd=3, yaxs="i",xlim=c(1950,2015))
  axis(1, at=seq(1950,2015,20), line=0, cex.axis=1, padj=-0.75, tck=-0.015)
  axis(2, at=yy, line=-0.45, cex.axis=1, padj=1, tck=-0.015)
  mtext(expression(paste("Smoothed spawners (log"[e]," scale)")), side=2, line=1.15, cex=0.75)
  mtext("Year", side=1, line=1.5, cex=0.75)
  points(Year ,pred.smooth.spw, lwd=2, col="dark grey",type="l",lty=1)
  points(Year12 ,pred.smooth.spw12, lwd=2, col="blue",type="l",lty=1)
  
  #add legend to plot
  par(mar=c(0,0,0,0),new=TRUE) 
  x<-0:1;y<-0:1;plot(x,y,xaxt="n",yaxt="n",type="n",bty="n")
  legend(x=0.125, y=1, legend=c("Trend - all generations", "Trend - last 3 generations"), col = c("dark gray","blue"), lty=1, lwd=2, bty="n", cex=0.85, ncol=1, seg.len=0.65)
  
  #add plot label
  mtext("(d)", side=3, line=-1.5, adj=0.05, cex=1)
  
  #end of inner if  else loop
    }
  #end of outer if  else loop
  }
  #end of function
}


