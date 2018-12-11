
# So what he has asked to do is that : 
#1 we have to calculate interleave keypress for EACH participants in singleDialing2
#2 Then, we calculate the mean of all these mmeans that we obtained.



# its specified that we have to consider that data for which no dialing error was made and so :
no_error <- subset(keyPressDataWithLaneDeviation, keyPressDataWithLaneDeviation$typingErrorMadeOnTrial == 0)

no_error_single_dialing <- subset(no_error, no_error$partOfExperiment == 'singleDialing2')


#to calculate the average Interkeypress for each participant 
#1
avg_InterkeyPress_foreachParticipant <- with(no_error_single_dialing, aggregate((timeRelativeToTrialStart / 1000) ~ (pp), no_error_single_dialing, mean))

#2
mean(avg_InterkeyPress_foreachParticipant$`timeRelativeToTrialStart/1000`)

# So what i understood we should do is that , this mean will be replaced in SingleTaskKeyPress at chunk intervals 1 and 5 and then .........we have to run the model for the 4th i guess
#also in the 4th remember to replace the guassDeviateSD to what ever SD we got in  the 2nd question ...i ran it with the standard value

