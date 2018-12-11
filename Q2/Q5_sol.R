generateComplexStrategies <- function()
{
    phoneStringLength <- 11
    numberform <- "11111111111"
    
    intervaltable <- expand.grid(0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 0:1, 0:1)
    intervalnum <- length(intervaltable[1,])
    strategiesnum <- length(intervaltable[,1])
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
        strategies <- rbind(strategies, df)
    }

    View(strategies)

    view_strat <- strategies

   

}



