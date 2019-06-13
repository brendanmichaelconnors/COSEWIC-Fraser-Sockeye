#0. Create table to populate
tableNums <- matrix(NA, 48, 13)
colnames(tableNums) <- c("DUID", "Proposed DU Name", "Generation Length", "Year Range", "Median%", "97.5%", "2.5%", "p|30", "p|50","p|70", "Num of Observations", "Est EFS Abundance","Mature")

for (g in 1:24)
{

#1. import raw data and subset for DU

dataRaw4 <- read.csv("data/Fr SK EFS data for B.Conners_LongForm.UPDATE.csv")
DU_list <- read.csv("data/DU_Names_List.csv", header = F)
DU_Names <-as.vector(DU_list[,2])

data4<-dataRaw4[which(dataRaw4$DUID == g),]

if (nrow(data4)==0) {
  
  
  tableNums[g,1] <- g
  tableNums[g,2] <- DU_Names[g]
  tableNums[g,3] <- NA
  tableNums[g,4] <- NA
  tableNums[g,5] <- NA
  tableNums[g,6] <- NA
  tableNums[g,7] <- NA
  tableNums[g,8] <- NA
  tableNums[g,9] <- NA
  tableNums[g,10] <- NA
  tableNums[g,11] <- NA
  tableNums[24+g,1] <- g
  tableNums[24+g,2] <- DU_Names[g]
  tableNums[24+g,3] <- NA
  tableNums[24+g,4] <- NA
  tableNums[24+g,5] <- NA
  tableNums[24+g,6] <- NA
  tableNums[24+g,7] <- NA
  tableNums[24+g,8] <- NA
  tableNums[24+g,9] <- NA
  tableNums[24+g,10] <- NA
  tableNums[24+g,11] <- NA
  tableNums[24+g,12] <- NA
  tableNums[24+g,13] <- NA
  
} else {

  
  #inner if else loop
  if(all(is.na(data4$EFS))==TRUE){
    
    tableNums[g,1] <- g
    tableNums[g,2] <- DU_Names[g]
    tableNums[g,3] <- NA
    tableNums[g,4] <- NA
    tableNums[g,5] <- NA
    tableNums[g,6] <- NA
    tableNums[g,7] <- NA
    tableNums[g,8] <- NA
    tableNums[g,9] <- NA
    tableNums[g,10] <- NA
    tableNums[g,11] <- NA
    tableNums[g,12] <- NA
    tableNums[g,13] <- NA
    tableNums[24+g,1] <- g
    tableNums[24+g,2] <- DU_Names[g]
    tableNums[24+g,3] <- NA
    tableNums[24+g,4] <- NA
    tableNums[24+g,5] <- NA
    tableNums[24+g,6] <- NA
    tableNums[24+g,7] <- NA
    tableNums[24+g,8] <- NA
    tableNums[24+g,9] <- NA
    tableNums[24+g,10] <- NA
    tableNums[24+g,11] <- NA
    tableNums[24+g,12] <- NA
    tableNums[24+g,13] <- NA
    
  } else {  
    
    
data4log <- log(data4[4])
  
  #generate smoothed EFS = 4yr running average (same as Gant et al. 2011 -- using 1yr before and 2yrs after)
  data4SS <- rollapply(data4log, 4, mean)
  
  #remove first yr and last 2yrs from Year because cannot have 4yr average for those years
  M3 <- nrow(data4)-2
  Year <- data4[2:M3,3] +2
  SS <- data4SS[,1]
  
  #subset data for most recent 13 yrs
  Year12 <- Year[(length(Year)-12):(length(Year))]
  SS12 <- SS[(length(Year)-12):(length(Year))]
  
  runif(1) # this is because some PCs need the seed to be initialized
  
#2. specify linear model in BUGS language
  model = paste("
	
					model {
							for (i in 1:N){
							y[i] ~ dnorm(y.hat[i], tau)
							y.hat[i] <- a + b * x[i]
						}
						a ~ dnorm(0, 0.000001)
							b ~ dnorm(0, 0.000001)
						tau <- pow(sigma, -2)
						sigma ~ dunif(0, 1000)
					}

			")
  cat(model, file = "linear.escape.bug")# this just saves the model as a BUGS file that will be called by JAGS in next step

#3. fit model to all years of data and last 12      
  #run model (using all yrs)
  bay.linear= jags(data = list("x"= Year,"y"= SS,"N"= length(SS)), inits = , parameters.to.save= c("a","b"), 
                   model.file="linear.escape.bug", n.chains = 1, n.burnin = 5000, n.thin = 5, n.iter = 100000, DIC = TRUE)
  out<-as.mcmc(bay.linear)
  
  #run model (using most recent 12 yrs)
  bay.linear= jags(data = list("x"= Year12,"y"= SS12,"N"= length(SS12)), inits = , parameters.to.save= c("a","b"), 
                   model.file="linear.escape.bug", n.chains = 1, n.burnin = 5000, n.thin = 5, n.iter = 100000, DIC = TRUE)
  out12<-as.mcmc(bay.linear)
 
  
  
  
#4. estimate changes in raw space and export in a table     

  ###All years###
  #estimate median (and 2.5th and 97.5th percentile) rate of change in log-e space
  raw.change.medianALL <- quantile(((exp(out[[1]][,2]*(length(SS12)-1)))-1)*100,probs=0.5)
  raw.change.upperALL <- quantile(((exp(out[[1]][,2]*(length(SS12)-1)))-1)*100,probs=0.975)
  raw.change.lowerALL <- quantile(((exp(out[[1]][,2]*(length(SS12)-1)))-1)*100,probs=0.025)
  
  #estimate p|30 & p|50
  p30_all <- (sum(out[[1]][,2] < (log(1-0.3)/(length(SS12)-1)))/length(out[[1]][,2]))*100
  p50_all <- (sum(out[[1]][,2] < (log(1-0.5)/(length(SS12)-1)))/length(out[[1]][,2]))*100
  p70_all <- (sum(out[[1]][,2] < (log(1-0.7)/(length(SS12)-1)))/length(out[[1]][,2]))*100
  
  #export percentils and probabilities to csv file
  tableNums[g,1] <- g
  tableNums[g,2] <- DU_Names[g]
  tableNums[g,3] <- c("4 yrs")
  tableNums[g,4] <- c("1954-2015")
  tableNums[g,5] <- round( raw.change.medianALL, digits = 0)
  tableNums[g,6] <- round(raw.change.upperALL, digits = 0)
  tableNums[g,7] <- round(raw.change.lowerALL, digits = 0)
  tableNums[g,8] <- round(p30_all/100, digits = 2)
  tableNums[g,9] <- round(p50_all/100, digits = 2)
  tableNums[g,10] <- round(p70_all/100, digits = 2)
  tableNums[g,11] <- length(SS12)
  tableNums[g,12] <- "-"
  
  ###Just last 12 years###
  #estimate median (and 2.5th and 97.5th percentile) rate of change in loge space
  raw.change.median12 <- quantile(((exp(out12[[1]][,2]*(length(SS12)-1)))-1)*100,probs=0.5)
  raw.change.upper12 <- quantile(((exp(out12[[1]][,2]*(length(SS12)-1)))-1)*100,probs=0.975)
  raw.change.lower12 <- quantile(((exp(out12[[1]][,2]*(length(SS12)-1)))-1)*100,probs=0.025)
  
  #estimate p|30 & p|50
  p30_12 <- (sum(out12[[1]][,2] < (log(1-0.3)/(length(SS12)-1)))/length(out12[[1]][,2]))*100
  p50_12 <- (sum(out12[[1]][,2] < (log(1-0.5)/(length(SS12)-1)))/length(out12[[1]][,2]))*100
  p70_12 <- (sum(out12[[1]][,2] < (log(1-0.7)/(length(SS12)-1)))/length(out12[[1]][,2]))*100
  
  #put percentiles and probabilities in matrix for export in panel 6
  tableNums[(24+g),1] <- g
  tableNums[(24+g),2] <- DU_Names[g]
  tableNums[(24+g),3] <- c("4 yrs")
  tableNums[(24+g),4] <- c("2003-2015")
  tableNums[(24+g),5] <- round(raw.change.median12, digits = 0)
  tableNums[(24+g),6] <- round(raw.change.upper12, digits = 0)
  tableNums[(24+g),7] <- round(raw.change.lower12, digits = 0)
  tableNums[(24+g),8] <- round(p30_12/100, digits = 2)
  tableNums[(24+g),9] <- round(p50_12/100, digits = 2)
  tableNums[(24+g),10] <- round(p70_12/100, digits = 2)
  tableNums[(24+g),11] <- length(SS12)
  tableNums[(24+g),12] <- exp(tail(data4SS, 1))
  #end of inner ifelse function 

  #Estiamte total mature individuals
    #import raw data and subset for DU
  dataRaw2 <- read.csv("data/skall_update.csv")

  ##Subset data1 to include only the rows corresponding to the current working DUID identified in the panel.combine R document
  #If the DUID does not exist, will produce a blank plot
  data2<-dataRaw2[which(dataRaw2$DUID == g),]
  
  if (nrow(data2)==0) {
    
     
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
}

  tableNums[(24+g),13] <- geometric.mean(tail(data2full[,2]+data2full[,4], 4))



  }
  
  #end of outer ifelse function 
  }
  #end of loop
}



write.csv(tableNums, "output/Table Nums_AllDUs.csv")
