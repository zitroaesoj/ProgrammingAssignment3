rankhospital <- function(state, outcome, num = "best") {
	## Read outcome data
	## Check that state and outcome are valid
	## Return hospital name in that state with the given rank
	## 30-day death rate

	## read csv into dataframe
	myDataFrame <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	
	## suppress warning: NAs introduced by coercion
	suppressWarnings(myDataFrame[, 11] <- as.numeric(myDataFrame[, 11]))
	suppressWarnings(myDataFrame[, 17] <- as.numeric(myDataFrame[, 17]))
	suppressWarnings(myDataFrame[, 23] <- as.numeric(myDataFrame[, 23]))
	
	## dataframe with states and frequency
	myDataFrame.States <- data.frame(State = names(tapply(myDataFrame$State, myDataFrame$State, 
							length)), Freq = tapply(myDataFrame$State, myDataFrame$State, length))

	## remove name from rows
	rownames(myDataFrame.States) <- NULL
	
	## dataframe with outcomes and frequency	
	myDataFrame.Outcomes <- data.frame(Outcome = c("heart attack", "heart failure", "pneumonia"), 
			Col = c(11, 17, 23))
	
	## validate state
	if (nrow(myDataFrame.States[myDataFrame.States$State == state, ]) == 0) 
		stop("invalid state")

	## validate outcome
	if (nrow(myDataFrame.Outcomes[myDataFrame.Outcomes$Outcome == outcome, ]) == 0) 
		stop("invalid outcome")
	
	## dataframe with states and frequency
	myDataFrame.MatchedStates <- myDataFrame[myDataFrame$State == state, ]

	## outcome column we're going to get rank of
	MatchColumn <- myDataFrame.Outcomes[myDataFrame.Outcomes$Outcome == outcome, 2]

	## only keep states with complete outcome
	myDataFrame.MatchedStates <- myDataFrame.MatchedStates[complete.cases(myDataFrame.MatchedStates[, MatchColumn]), ]

	## sort by hospital name
	myDataFrame.MatchedStates <- myDataFrame.MatchedStates[order(myDataFrame.MatchedStates[, MatchColumn], myDataFrame.MatchedStates$Hospital.Name), ]

	## if num = best then just change to 1 
	if (num == "best") 
		num <- 1

	## if num = worst then just change to number of rows
	if (num == "worst") 
		num <- nrow(myDataFrame.MatchedStates)

	## suppress warning: NAs introduced by coercion
	## row that has num
	suppressWarnings(MatchRow <- as.numeric(num))

	return(myDataFrame.MatchedStates[MatchRow, ]$Hospital.Name)
}