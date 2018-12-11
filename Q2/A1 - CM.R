#assignement 1
#Question: 1.(A)
#subsetting
no_error <- subset(keyPressDataWithLaneDeviation,keyPressDataWithLaneDeviation$typingErrorMadeOnTrial==0)

no_error_dual_steer_focus <- subset(no_error, no_error$partOfExperiment == 'dualSteerFocus' )

no_error_dual_dial_focus <- subset(no_error, no_error$partOfExperiment == 'dualDialFocus' )

time_steer_focus <- subset(no_error_dual_steer_focus, no_error_dual_steer_focus$phoneNrLengthAfterKeyPress == 11)
time_dial_focus <- subset(no_error_dual_dial_focus, no_error_dual_dial_focus$phoneNrLengthAfterKeyPress == 11)

#Aggregating using with()

mean_steer_focus <- with(time_steer_focus,aggregate(timeRelativeToTrialStart ~ pp ,time_steer_focus,mean))

mean_dial_focus <- with(time_dial_focus,aggregate(timeRelativeToTrialStart ~ pp ,time_dial_focus,mean))

#Grand Mean, Standard Deviation, Standard Error

grand_mean_steer_focus <- mean(mean_steer_focus$timeRelativeToTrialStart)

grand_mean_dial_focus <- mean(mean_dial_focus$timeRelativeToTrialStart)

sd_steer_focus <- sd(mean_steer_focus$timeRelativeToTrialStart, na.rm = FALSE)

sd_dial_focus <- sd(mean_dial_focus$timeRelativeToTrialStart, na.rm = FALSE)

se_steer_focus <- sd_steer_focus/(sqrt(12))

se_dial_focus <- sd_dial_focus/(sqrt(12)) 

#Q (B) Lane deviation position of steerfocus and dial focus
abs_mean_lane_position_steer_focus <- mean(abs(no_error_dual_steer_focus$lanePosition))

abs_mean_lane_position_dial_focus <- mean(abs(no_error_dual_dial_focus$lanePosition))


#Q(C)


library(dplyr)




laneDev_steerFocus <- with(no_error_dual_steer_focus, select(no_error_dual_steer_focus, pp, lanePosition, timeRelativeToTrialStart, phoneNrLengthAfterKeyPress))

laneDev_dialFocus <- with(no_error_dual_dial_focus, select(no_error_dual_dial_focus, pp, lanePosition, timeRelativeToTrialStart, phoneNrLengthAfterKeyPress))

abs_lanePosition_steerFocus <- abs(laneDev_steerFocus)
abs_lanePosition_dialFocus <- abs(laneDev_dialFocus)

#plot(no_error_dual_steer_focus$phoneNrLengthAfterKeyPress & no_error_dual_steer_focus$pp=="1", abs_lanePosition_steerFocus, xlab = "Dialing time", ylab = "lanedeviation")



mean_time_for_SteerFocus_each_participant <- with(laneDev_steerFocus, aggregate(timeRelativeToTrialStart ~ pp ~ abs_mean_lane_position_steer_focus, laneDev_steerFocus, mean))



mean_time_for_dialFocus_each_participant <- with(laneDev_dialFocus, aggregate(timeRelativeToTrialStart ~ pp, laneDev_dialFocus, mean))








