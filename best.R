rankall <- function( outcome, num = "best") {

	states = c('AK','AL','AR','AS','AZ','CA','CO','CT','DC','DE','FL','GA','GU','HI','IA','ID','IL','IN','KS','KY','LA','MA','MD','ME','MH','MI','MN','MO','MS','MT','NC','ND','NE','NH','NJ','NM','NV','NY','OH','OK','OR','PA','PR','PW','RI','SC','SD','TN','TX','UT','VA','VI','VT','WA','WI','WV','WY');
#	states = c('a', 'b');
	valid <- FALSE;
	final <- data.frame(as.character(), as.character())
	names(final) <- c("hospital", "state")
	## Read outcome data
	outcomeFile <- read.csv("outcome-of-care-measures.csv", colClasses = "character");

	## Check that state and outcome are valid

	if (outcome !="heart attack" && outcome!="heart failure" && outcome!="pneumonia"){
		stop("invalid outcome");
	}


	if (outcome =="heart attack"){
		if(num=="best"){
			outcomeFile[,11] <- suppressWarnings(as.numeric(outcomeFile[,11]))
			for(i in states){
		  		z <- outcomeFile[outcomeFile$State == i,c(2,7, 11)]
				SortedList2 <- z[order(z[,3], z[,1], na.last = NA), ]
				names(SortedList2) <- c("hospital", "state", "Rate")
				if(length(SortedList2[,1])==0){
					SortedList2 <- data.frame(NA, i, NA)
					names(SortedList2) <- c("hospital", "state", "Rate")
					final <- rbind(final, SortedList2[1,1:2])
				}	
				else{
					final <- rbind(final, SortedList2[1,1:2])					
				}
			}
		}
		else if(num=="worst"){
			outcomeFile[,11] <- suppressWarnings(as.numeric(outcomeFile[,11]))
			for(i in states){
		  		z <- outcomeFile[outcomeFile$State == i,c(2,7, 11)]
				SortedList2 <- z[order(-z[,3], z[,1], na.last = NA), ]
				names(SortedList2) <- c("hospital", "state", "Rate")
				if(length(SortedList2[,1])==0){
					SortedList2 <- data.frame(NA, i, NA)
					names(SortedList2) <- c("hospital", "state", "Rate")
					final <- rbind(final, SortedList2[1,1:2])
				}	
				else{
					final <- rbind(final, SortedList2[1,1:2])					
				}
			}
		}
		else{
			outcomeFile[,11] <- suppressWarnings(as.numeric(outcomeFile[,11]))
			for(i in states){
		  		z <- outcomeFile[outcomeFile$State == i,c(2,7, 11)]
				SortedList2 <- z[order(z[,3], z[,1]), ]
				names(SortedList2) <- c("hospital", "state", "Rate")

				if(length(SortedList2[,1])==0){
					SortedList2 <- data.frame(NA, i, NA)
					names(SortedList2) <- c("hospital", "state", "Rate")
					final <- rbind(final, SortedList2[1,1:2])
				}	
				else{
					final <- rbind(final, SortedList2[num,1:2])					
				}
			}
		}
	}
	if (outcome =="heart failure"){
		if(num=="best"){
			outcomeFile[,17] <- suppressWarnings(as.numeric(outcomeFile[,17]))
			for(i in states){
		  		z <- outcomeFile[outcomeFile$State == i,c(2,7, 17)]
				SortedList2 <- z[order(z[,3], z[,1], na.last = NA), ]
				names(SortedList2) <- c("hospital", "state", "Rate")
				if(length(SortedList2[,1])==0){
					SortedList2 <- data.frame(NA, i, NA)
					names(SortedList2) <- c("hospital", "state", "Rate")
					final <- rbind(final, SortedList2[1,1:2])
				}	
				else{
					final <- rbind(final, SortedList2[1,1:2])					
				}
			}
		}
		else if(num=="worst"){
			outcomeFile[,17] <- suppressWarnings(as.numeric(outcomeFile[,17]))
			for(i in states){
		  		z <- outcomeFile[outcomeFile$State == i,c(2,7, 17)]
				SortedList2 <- z[order(-z[,3], z[,1], na.last = NA), ]
				names(SortedList2) <- c("hospital", "state", "Rate")
				if(length(SortedList2[,1])==0){
					SortedList2 <- data.frame(NA, i, NA)
					names(SortedList2) <- c("hospital", "state", "Rate")
					final <- rbind(final, SortedList2[1,1:2])
				}	
				else{
					final <- rbind(final, SortedList2[1,1:2])					
				}
			}
		}
		else{
			outcomeFile[,17] <- suppressWarnings(as.numeric(outcomeFile[,17]))
			for(i in states){
		  		z <- outcomeFile[outcomeFile$State == i,c(2,7, 17)]
				SortedList2 <- z[order(z[,3], z[,1], na.last = NA), ]
				names(SortedList2) <- c("hospital", "state", "Rate")
				if(length(SortedList2[,1])==0){
					SortedList2 <- data.frame(NA, i, NA)
					names(SortedList2) <- c("hospital", "state", "Rate")
					final <- rbind(final, SortedList2[1,1:2])
				}	
				else{
					final <- rbind(final, SortedList2[num,1:2])					
				}
			}
		}
	}
	if (outcome =="pneumonia"){
		if(num=="best"){
			outcomeFile[,23] <- suppressWarnings(as.numeric(outcomeFile[,23]))
			for(i in states){
		  		z <- outcomeFile[outcomeFile$State == i,c(2,7, 23)]
				SortedList2 <- z[order(z[,3], z[,1], na.last = NA), ]
				names(SortedList2) <- c("hospital", "state", "Rate")
				if(length(SortedList2[,1])==0){
					SortedList2 <- data.frame(NA, i, NA)
					names(SortedList2) <- c("hospital", "state", "Rate")
					final <- rbind(final, SortedList2[1,1:2])
				}	
				else{
					final <- rbind(final, SortedList2[1,1:2])					
				}
			}
		}
		else if(num=="worst"){
			outcomeFile[,23] <- suppressWarnings(as.numeric(outcomeFile[,23]))
			for(i in states){
		  		z <- outcomeFile[outcomeFile$State == i,c(2,7, 23)]
				SortedList2 <- z[order(-z[,3], z[,1], na.last = NA), ]
				names(SortedList2) <- c("hospital", "state", "Rate")
				if(length(SortedList2[,1])==0){
					SortedList2 <- data.frame(NA, i, NA)
					names(SortedList2) <- c("hospital", "state", "Rate")
					final <- rbind(final, SortedList2[1,1:2])
				}	
				else{
					final <- rbind(final, SortedList2[1,1:2])					
				}
			}
		}
		else{
			outcomeFile[,23] <- suppressWarnings(as.numeric(outcomeFile[,23]))
			for(i in states){
		  		z <- outcomeFile[outcomeFile$State == i,c(2,7, 23)]
				SortedList2 <- z[order(z[,3], z[,1], na.last = NA), ]
				names(SortedList2) <- c("hospital", "state", "Rate")
				if(length(SortedList2[,1])==0){
					SortedList2 <- data.frame(NA, i, NA)
					names(SortedList2) <- c("hospital", "state", "Rate")
					final <- rbind(final, SortedList2[1,1:2])
				}	
				else{
					final <- rbind(final, SortedList2[num,1:2])					
				}
			}
		}
	}
	final

}