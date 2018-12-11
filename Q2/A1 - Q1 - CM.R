#ASSIGNMENT 1
#Question: 1.(A)

#Subsetting

no_error <- subset(keyPressDataWithLaneDeviation,keyPressDataWithLaneDeviation$typingErrorMadeOnTrial == 0)

no_error_dual_steer_focus <- subset(no_error, no_error$partOfExperiment == 'dualSteerFocus' )

no_error_dual_dial_focus <- subset(no_error, no_error$partOfExperiment == 'dualDialFocus' )

time_steer_focus <- subset(no_error_dual_steer_focus, no_error_dual_steer_focus$phoneNrLengthAfterKeyPress == 11)

time_dial_focus <- subset(no_error_dual_dial_focus, no_error_dual_dial_focus$phoneNrLengthAfterKeyPress == 11)

#Mean of the dialing time for the 2 experimental conditions

mean_time_steer_focus <- with(time_steer_focus,aggregate(timeRelativeToTrialStart ~ pp ,time_steer_focus,mean))

mean_time_dial_focus <- with(time_dial_focus,aggregate(timeRelativeToTrialStart ~ pp ,time_dial_focus,mean))

#Grand Mean, Standard Deviation, Standard Error of the dialing time for the 2 experimental conditions

grand_mean_time_steer_focus <- mean(mean_time_steer_focus$timeRelativeToTrialStart)

grand_mean_time_dial_focus <- mean(mean_time_dial_focus$timeRelativeToTrialStart)

sd_time_steer_focus <- sd(mean_time_steer_focus$timeRelativeToTrialStart, na.rm = FALSE)

sd_time_dial_focus <- sd(mean_time_dial_focus$timeRelativeToTrialStart, na.rm = FALSE)

se_time_steer_focus <- sd_time_steer_focus/(sqrt(12))

se_time_dial_focus <- sd_time_dial_focus/(sqrt(12)) 

#Question: 1.(B)

#Grand mean of the absolute lane deviation for the 2 experimental conditions

mean_lane_position_steer_focus <- mean(abs(no_error_dual_steer_focus$lanePosition))

mean_lane_position_dial_focus <- mean(abs(no_error_dual_dial_focus$lanePosition))

#Question: 1.(C)

#Mean of the absolute lane deviation for each digit for each participant for the 2 experimental conditions

mean_pp_lane_deviation_steer_focus <- with(no_error_dual_steer_focus,aggregate(abs(lanePosition) ~ pp + phoneNrLengthAfterKeyPress ,no_error_dual_steer_focus, mean))

mean_pp_lane_deviation_dial_focus <- with(no_error_dual_dial_focus,aggregate(abs(lanePosition) ~ pp + phoneNrLengthAfterKeyPress ,no_error_dual_dial_focus, mean))

#Grand mean of the absolute lane deviation for each digit for the 2 experimental conditions

grand_mean_lane_deviation_steer_focus <- with(mean_pp_lane_deviation_steer_focus,aggregate( `abs(lanePosition)` ~ phoneNrLengthAfterKeyPress ,mean_pp_lane_deviation_steer_focus, mean))

grand_mean_lane_deviation_dial_focus <- with(mean_pp_lane_deviation_dial_focus,aggregate( `abs(lanePosition)` ~ phoneNrLengthAfterKeyPress ,mean_pp_lane_deviation_dial_focus, mean))

#Plot Lane Deviation over Time

par(ann=FALSE)

plot(grand_mean_lane_deviation_steer_focus$phoneNrLengthAfterKeyPress,grand_mean_lane_deviation_steer_focus$`abs(lanePosition)`,pch=19,col=2,type = 'o') 

par(new=TRUE,ann=FALSE)

plot(grand_mean_lane_deviation_dial_focus$phoneNrLengthAfterKeyPress,grand_mean_lane_deviation_dial_focus$`abs(lanePosition)`,pch=15,col=3,type = 'o')

title(main = 'Lane Deviation over Time',xlab = 'Phone N° lenght after keypress',ylab = 'Lane Deviation (m)')



#title(main = 'Lane deviation over time',xlab = 'Dialing Time (sec)',ylab = 'Lane Deviation (m)')

#lines(c(0,10),c(10,0),col=3)

#points(rnorm(15,5,0.5),rnorm(15,4,0.5),col=2,pch=20) 

#text(5,9, "test")


#plot((0:2) ^ 4 , (0:2) ^ 0.2, main = "plot")



#Question: 1.(D)   Read carefully the article before answering!!


































