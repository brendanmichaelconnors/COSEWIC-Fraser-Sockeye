# LOAD NECESSARY PACKAGES
library(plotrix)
library(R2jags)
library(zoo)
library(psych)
# LOOP THROUGH EACH DU TO GENERATE 6 PANEL PLOT

for (g in 1:24){
  source("panel 1.r")
  source("panel 2.r")
  source("panel 3.r")
  source("panel 4.r")
  source("panel 5.r")
  source("panel 6.r")
  
  mypath <- file.path("output/",paste("DU_6panel_", g, ".pdf", sep = ""))
  pdf(file=mypath, width=6.5, height=7.5)
  
    par(mfrow=c(3,2), bty="o", mar=c(0,0,0,0))   
    
    Panel1(data1)
    Panel2(data2)
    Panel3(data3)
    Panel4(data4)
    Panel5(data5)
    Panel6(data6)
  
  dev.off()
}


# LOOP THROUGH EACH DU TO GENERATE 6 PANEL PLOT

for (g in 1:24){
  source("panel 7.r")
  source("panel 8.r")
    
  mypath <- file.path("output/",paste("DU_2panel_", g, ".pdf", sep = ""))
  pdf(file=mypath, width=6.5, height=3)
  
    par(mfrow=c(1,2), bty="o", mar=c(0,0,0,0))
    
    Panel7(data7)
    Panel8(data8)
    
  dev.off()
}

close.screen(all.screens = TRUE)


#EXPORT TABLE OF % CHANGE OVER TIME
source("change table.r")
