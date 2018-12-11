


AllComplexStratergies <- function() # so, This is the function that generates combinations along with interleaving 

# we are testing it here with a fixed fone number
{
    phoneStringLength <- 11
    numberform <- "11111111111"

    intervaltable <- expand.grid(0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 0:1)
    intervalnum <- length(intervaltable[1,])
    strategiesnum <- length(intervaltable[, 1])
    intersplit <- character()
    strategy <- numeric()
    strategies <- data.frame("interleave pattern" = intersplit, "strategy" = strategy)

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
                lhs <- paste0('[1]', i, '[1-]')
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

        print(strategy)

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
        strategies <- rbind(strategies, df) #The combination is stored in "df"

    }

    View(strategies)

    ComplexStrategiestable <- strategies
}

RunAllComplexStratergies <- function(nrSimulation,phoneNumber)
{

    #source(AllComplexStratergies,local = TRUE) # This was just an attempt in making this work 
    velocity <- c()



    #normalPhoneStructure <- c(1, 6) ### indicate at what digit positions a chunk needs to be retrieved (1st and 6th digit)
    phoneStringLength <- 11 ### how many digits does the number have?


    ### vectors that will contain output of the simulation. These are later used to create 1 table with all values
    keypresses <- c()
    times <- c()
    deviations <- c()
    strats <- c()
    steers <- c()
    allSamplesTable <- data.frame("strats" = character(),"steers" = numeric(),"x" = numeric(),"TrialTime" = numeric())
 
    #importing all stratergies :: we are importing all these from the above function
    complex_strategies <- ComplexStrategiestable$Strategy 
    complex_strategies_pattern <- as.character(ComplexStrategiestable$InterLeavePattern)
    totalStratNumber <- length(complex_strategies) 
    
    #for( a in 1 : totalStratNumber)
        for(a in 1:1) #to test simulation for one stratergy 
        {
            strat <- complex_strategies[[a]]
            pat <- complex_strategies_pattern[a]
            localSteerTimeOptions <- steeringTimeOptions
            
        
        if(length(strat) == 0)
                {
                
                    localSteerTimeOptions <- c(0)        
                }


    #from here on its like chris coded in RunAllSimpleStratergies

        for (steerTimes in locSteerTimeOptions)
        {
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
                strats <- c(strats, rep(nrDigitsPerTime, nrow(locTab)))
                steers <- c(steers, rep(steerTimes, nrow(locTab)))

            }
        }
        #end of for steerTimes	

    }
    ##end of for nr strategies


    ### now make a new table based on all the data that was collected
    tableAllSamples <- allSamplesTable
    tableAllSamples <- data.frame(keypresses, times, deviations, strats, steers)


    #### In the table we collected data for multiple simulations per strategy. Now we want to know the average performane of each strategy.
    #### These aspects are calculated using the "aggregate" function


    ## calculate average deviation at each keypress (keypresses), for each unique strategy variation (strats and steers)
    agrResults <- with(tableAllSamples, aggregate(deviations, list(keypresses = keypresses, strats = strats, steers = steers), mean))
    agrResults$dev <- agrResults$x


    ### also calculate the time interval
    agrResults$times <- with(tableAllSamples, aggregate(times, list(keypresses = keypresses, strats = strats, steers = steers), mean))$x


    ###now calculate mean drift across the trial
    agrResultsMeanDrift <- with(agrResults, aggregate(dev, list(strats = strats, steers = steers), mean))
    agrResultsMeanDrift$dev <- agrResultsMeanDrift$x

    ### and mean trial time
    agrResultsMeanDrift$TrialTime <- with(agrResults[agrResults$keypresses == 11,], aggregate(times, list(strats = strats, steers = steers), mean))$x


    #### make a plot that visualizes all the strategies: note that trial time is divided by 1000 to get the time in seconds
    with(tableAllSamples, plot(TrialTime / 1000, abs(x), pch = 21, bg = "dark grey", col = "dark grey", log = "x", xlab = "Dial time (s)", ylab = "Average Lateral Deviation (m)"))
   # save(tableAllSamples, file = "Q5data.Rda")

    ### give a summary of the data	
    s <- summary(agrResultsMeanDrift$TrialTime)

}

RunAllComplexStratergies(3, "11111111111") # but this isnt working ....i dont understand why 
        


 




