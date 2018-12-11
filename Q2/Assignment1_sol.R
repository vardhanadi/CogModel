
#assignement 1
#Question: 1.(A)
#subsetting
no_error <- subset(keyPressDataWithLaneDeviation,keyPressDataWithLaneDeviation$typingErrorMadeOnTrial==0)
#tot_dial_time <- subset(keyPressDataWithLaneDeviation,keyPressDataWithLaneDeviation$Event1=="Correct")



no_error_dual_steer_focus <- subset(no_error, no_error$partOfExperiment == 'dualSteerFocus' )

no_error_dual_dial_focus <- subset(no_error, no_error$partOfExperiment == 'dualDialFocus' )

time_steer_focus <- subset(no_error_dual_steer_focus, no_error_dual_steer_focus$phoneNrLengthAfterKeyPress == 11)
time_dial_focus <- subset(no_error_dual_dial_focus, no_error_dual_dial_focus$phoneNrLengthAfterKeyPress == 11)

#calc mean sd and se for steer focus

ag_steer_focus_mean <- with(time_steer_focus, aggregate(timeRelativeToTrialStart ~ pp, time_steer_focus, mean))
ag_steer_focus_standdev <- with(time_steer_focus, aggregate(timeRelativeToTrialStart ~ pp, time_steer_focus, sd))

grand_mean_steerfocus <- mean(ag_steer_focus_mean$timeRelativeToTrialStart)


sd_steerfocus <- sd(ag_steer_focus_mean$timeRelativeToTrialStart,na.rm = FALSE)



standard_error_steerFocus <- sqrt(grand_mean_steerfocus/sd_steerfocus)

#calc mean sd and se for dial focus 

ag_dial_focus_mean <- with(time_dial_focus, aggregate(timeRelativeToTrialStart ~ pp, time_dial_focus, mean))
ag_dial_focus_standdev <- with(time_dial_focus, aggregate(timeRelativeToTrialStart ~ pp, time_dial_focus, sd))

grand_mean_dialfocus <- mean(ag_dial_focus_mean$timeRelativeToTrialStart)

standard_error_dialFocus <- sqrt(ag_dial_focus_mean/ag_dial_focus_standdev)


#Q (B) Lane deviation position of steerfocus and dial focus
abs_mean_lane_position_steer_focus <- mean(abs(no_error_dual_steer_focus$lanePosition))

abs_mean_lane_position_dial_focus <- mean(abs(no_error_dual_dial_focus$lanePosition))

#Question C: make use of R's "aggregate" function to calculate averages first per
#participant per digit typed and condition. Once you have those averages, then average across participants.
# Question C:for lane deviation, you want to take the absolute value of lanedeviation.
#Question C:You can use the column “phoneNrLengthAfterKeyPress” to knowhow absolute lane deviation develops over time.#Question C:fornice plotting, you might want to look at the help files of "plot", "points", "lines", "par", and "legend"

#Q(C)


library(dplyr)

#hint 1 
abs_lanePosition_steerFocus <- abs(no_error_dual_steer_focus$lanePosition)

abs_lanePosition_dialFocus <- abs(no_error_dual_dial_focus$lanePosition)

#Hint 2
#list_steerFocus_lenAfterKeyPress <- list(no_error_dual_steer_focus$pp, abs_lanePosition_steerFocus, no_error_dual_steer_focus$phoneNrLengthAfterKeyPress)


#list_dialFocus_lenAfterKeyPress <- list(no_error_dual_dial_focus$pp, abs_lanePosition_dialFocus, no_error_dual_steer_focus$phoneNrLengthAfterKeyPress)

plot(no_error_dual_steer_focus$phoneNrLengthAfterKeyPress, abs_lanePosition_steerFocus, xlab = "Dialing time", ylab = "lanedeviation")















laneDev_steerFocus <- with(no_error_dual_steer_focus, select(no_error_dual_steer_focus, pp, lanePosition, timeRelativeToTrialStart, phoneNrLengthAfterKeyPress))

laneDev_dialFocus <- with(no_error_dual_dial_focus, select(no_error_dual_dial_focus, pp, lanePosition, timeRelativeToTrialStart, phoneNrLengthAfterKeyPress))



mean_time_for_SteerFocus_each_participant <- with(laneDev_steerFocus, aggregate(timeRelativeToTrialStart ~ pp ~ abs_mean_lane_position_steer_focus, laneDev_steerFocus, mean))



mean_time_for_dialFocus_each_participant <- with(laneDev_dialFocus, aggregate(timeRelativeToTrialStart ~ pp, laneDev_dialFocus, mean))























