pollutantmean <- function(directory, pollutant, id = 1:332) {
        a <- data.frame();
        for (i in id){
                if(i<10)
                        z <- paste(directory, "/00", i,".csv", sep="")
                else if(i>=10 && i <100)
                        z <- paste(directory, "/0", i,".csv", sep="")
                else
                        z <- paste(directory, "/", i,".csv", sep="")

                x <- read.csv(z);
                a <- rbind(a, x);
#                print(x[!is.na(x)])
        }
        myMean <- mean(a[,pollutant], na.rm=TRUE);
        specify_decimal <- function(x, k) format(round(x, k), nsmall=k)
	        as.numeric(specify_decimal(myMean, 3));
}