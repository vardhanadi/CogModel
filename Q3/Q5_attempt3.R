generateComplexStrategies <- function()
{
    phoneStringLength <- 11
    numberform <- "11111111111"
    intervaltable <- expand.grid(0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 0:1)
    intervalnum <- length(intervaltable[1,])
    strategiesnum <- length(intervaltable[, 1])
    intersplit <- character()
    strategy <- numeric()
    strategies <- data.frame("interleave pattern" = character(), "strategy" = numeric())

    for (k in 1:strategiesnum)
    {
        strategy <- c()
        str <- numberform

        print(intervaltable[k,])
        for (i in intervalnum:1)
        {
            if (intervaltable[k, ][i] == 1)
            {
                interleave <- '-'
                lhs <- paste0('^([1]{', i, '})([1-]+)$')
                rhs <- paste0('\\1', interleave, '\\2')
                str <- gsub(lhs, rhs, str)
            }
        }
        print(str)
        intersplit <- strsplit(str, "-")

        for (i in 1:length(intersplit[[1]]))
        {
            strategy <- c(strategy, nchar(intersplit[[1]][i]))
        }
        #print(strategy)
        position <- 0
        for (i in 1:length(strategy))
        {
            position <- strategy[i] + position
            strategy[i] <- position
        }

        ### remove last digit, as driver does not interleave after typing the last digit (they are done with the trial :-)  )
        strategy <- strategy[strategy != phoneStringLength]

        df <- data.frame("InterLeavePattern" = str, "Strategy" = I(list(strategy)))
        print(df)
        strategies <- rbind(strategies, df)
    }
    View(strategies)
    ComplexStrategiestable <- strategies


    strategies
}    


runAllComplexStrategies <- function(nrSimulations, phoneNumber)
{

    normalPhoneStructure <- c(1, 6) ### indicate at what digit positions a chunk needs to be retrieved (1st and 6th digit)
    phoneStringLength <- 11 ### how many digits does the number have?

    ### vectors that will contain output of the simulation. These are later used to create 1 table with all values
    keypresses <- c()
    times <- c()
    deviations <- c()
    strats <- c()
    steers <- c()
    tableAllSamples <- data.frame("strats" = character(), "steers" = numeric(), "x" = numeric(), "TrialTime" = numeric())
    #get all possible interleave strategies

    ComplexStrategies <- ComplexStrategiestable$Strategy
    ComplexStrategiesPattern <- as.character(ComplexStrategiestable$InterLeavePattern)
    totalstratnum <- length(ComplexStrategies)

    #ComplexStrategies is a list 

    ### iterate through all strategies

    for (j in 1:totalstratnum)
        #for (j in 1:1)
    {

        #get on strategy here
        strategy <- ComplexStrategies[[j]]
        pattern <- ComplexStrategiesPattern[j]

        locSteerTimeOptions <- steeringTimeOptions
        if (length(strategy) == 0)
        {
            locSteerTimeOptions <- c(0)
        }


        ### now run a trial (runOneTrial) for all combinations of how frequently you update the steering when you are steering (locSteerTimeOptions) and for the nuber of simulations that you want to run for each strategy (nrSimulations)
        for (steerTimes in locSteerTimeOptions)
        {
            keypresses <- c()
            times <- c()
            deviations <- c()
            strats <- c()
            steers <- c()

            #run all trails
            for (i in 1:nrSimulations)
            {

                ### run the simulation and store the output in a table
                locTab <- runOneTrial(strategy, steerTimes, normalPhoneStructure, phoneStringLength, phoneNumber)

                ##only look at rows where there is a keypress
                locTab <- locTab[locTab$events == "keypress",]

                ### add the relevant data points to variables that are stored in a final table
                keypresses <- c(keypresses, 1:nrow(locTab))
                times <- c(times, locTab$times)
                deviations <- c(deviations, locTab$drifts)
                strats <- c(strats, rep(pattern, nrow(locTab)))
                steers <- c(steers, rep(steerTimes, nrow(locTab)))
            }

            tableOneSetSamples <- data.frame(keypresses, times, deviations, strats, steers)
            View(tableOneSetSamples)
            ## calculate average deviation at each keypress (keypresses), for each unique strategy variation (strats and steers)
            agrResults <- with(tableOneSetSamples, aggregate(deviations, list(keypresses = keypresses, strats = strats, steers = steers), mean))
            agrResults$dev <- agrResults$x
            # 
            ### also calculate the time interval
            agrResults$times <- with(tableOneSetSamples, aggregate(times, list(keypresses = keypresses, strats = strats, steers = steers), mean))$x

            ###now calculate mean drift across the trial
            agrResultsMeanDrift <- with(agrResults, aggregate(dev, list(strats = strats, steers = steers), mean))
            #agrResultsMeanDrift$dev <- agrResultsMeanDrift$x

            ### and mean trial time
            agrResultsMeanDrift$TrialTime <- with(agrResults[agrResults$keypresses == 11,], aggregate(times, list(strats = strats, steers = steers), mean))$x
            tableAllSamples <- rbind(tableAllSamples, agrResultsMeanDrift)
            #View(agrResultsMeanDrift)
        }
        #end of for steerTimes	

    }
    ##end of for nr strategies


    #### make a plot that visualizes all the strategies: note that trial time is divided by 1000 to get the time in seconds
    with(tableAllSamples, plot(TrialTime / 1000, abs(x), pch = 21, bg = "dark grey", col = "dark grey", log = "x", xlab = "Dial time (s)", ylab = "Average Lateral Deviation (m)"))
    save(tableAllSamples, file = "Q5data.Rda")
    ### give a summary of the data	
    summary(tableAllSamples$TrialTime)
}



Q5A <- function()
{
    load("Q5data.Rda")
    with(tableAllSamples, plot(x = TrialTime / 1000, y = abs(x), pch = 21, bg = "dark grey", col = "dark grey", log = "x", xlab = "Dial time (s)", ylab = "Average Lateral Deviation (m)", ylim = c(0.2, 0.7)))
    #with(tableAllSamples,plot(x = TrialTime/1000,y= abs(x),pch=21,bg="dark grey",col="dark grey",log="x",xlab="Dial time (s)",ylab="Average Lateral Deviation (m)"))

    #Steering F
    lines(5.03, 0.37, type = "o", pch = 0)
    arrows(5.03 - 0.5, 0.37, 5.03 + 0.5, 0.37, length = 0.05, angle = 90, code = 3)
    arrows(5.03, 0.37 - 0.1, 5.03, 0.37 + 0.1, length = 0.05, angle = 90, code = 3)

    #Dailing
    lines(3.51, 0.56, type = "o", pch = 5)
    arrows(3.51 - 0.4, 0.56, 3.51 + 0.4, 0.56, length = 0.05, angle = 90, code = 3)
    arrows(3.51, 0.56 - 0.15, 3.51, 0.56 + 0.15, length = 0.05, angle = 90, code = 3)
}