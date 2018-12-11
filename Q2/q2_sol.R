#Question 2 


library(graphics)
library(utils)
library(RColorBrewer)
library(ggplot2)
library(stats)


#A

#subsetting tableofDriftValues for sorting the data between 15 and 18sec to make a plot  
drift_value_t <- subset(tableOfDriftValuesCalibration,tableOfDriftValuesCalibration$trialTime > 15000 & tableOfDriftValuesCalibration$trialTime < 18000)
x <- drift_value_t$trialTime
y <- drift_value_t$posX


q2a <- function()
  {
  
  for (j in 1:50) 
  {
  

  b <- sort(rnorm(j,mean = 0.0,sd = 0.13),decreasing = FALSE)

t <- plot(x,y,xlab = "trialTime(ms)", ylab = "LateralPosition(m)",type = "b", col = palette()[1:6]) #the plot between Lateral Position(m) and TrialTime(ms)

hist(b, freq = 50, main = paste("Histogram fr q2a"), col = palette()[1:6])
}

  }

q2a()

#B
#To generate lateral position data for 20 simulated trials in which you assume care starts
#from point 0 and samples values from modelling distribution (M = 0 and SD = 0.13) every
#50 milliseconds for a period of 3000 milliseconds.

q2b <- function()
  {
  for (i in 1:50) 
    {
    
   f <- plot( a<- sort(rnorm(i, mean = 0,sd = 0.13),decreasing = FALSE),xlab = "trialTime(ms)", ylab = "LateralPosition(m)",type = "l", col = palette()[1:6], cex = 1)
                                                        #if this is the histohram produced then why isnt it fukin colorful   
   #Time for Histogramm:
   hist(a,freq = 50,main = paste("Histogram for q2B"), col = palette()[1:6])
   
    t <- cumsum(y)
    
    
     }
}

q2b()

#C 

q2c <- 