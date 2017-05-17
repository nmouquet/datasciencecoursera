# The zip file contains 332 comma-separated-value (CSV) files containing pollution monitoring data for fine particulate matter (PM) air pollution at 332 locations in the United States. Each file contains data from a single monitor and the ID number for each monitor is contained in the file name. For example, data for monitor 200 is contained in the file "200.csv". Each file contains three variables:
#   
# Date: the date of the observation in YYYY-MM-DD format (year-month-day)
# sulfate: the level of sulfate PM in the air on that date (measured in micrograms per cubic meter)
# nitrate: the level of nitrate PM in the air on that date (measured in micrograms per cubic meter)


#Part 1

#Write a function named 'pollutantmean' that calculates the mean of a pollutant 
#(sulfate or nitrate) across a specified list of monitors. 
#The function 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'. 
#Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' particulate 
#matter data from the directory specified in the 'directory' argument and returns 
#the mean of the pollutant across all of the monitors, ignoring any missing values 
#coded as NA. A prototype of the function is as follows

rm(list = ls())
pollutantmean <- function(directory,pollutant,id=1:332)
{
  setwd(directory)
  files <- list.files()
  mean_polut <- vector(length = length(id))
  for (i in 1:length(id)){
    data <- read.csv(files[id[i]])
    mean_polut[i] <- mean(data[,pollutant],na.rm=TRUE)
  }
  mean(mean_polut,na.rm=TRUE)
}

setwd('/Users/nmouquet/Documents/Math-Prog-Stat/R/coursera/datasciencecoursera/datasciencecoursera')
pollutantmean("specdata", "nitrate")

mean(c(0.255,1.43,2.36))

complete <- function (directory,id=1:332)
{
  #setwd('/Users/nmouquet/Documents/Math-Prog-Stat/R/coursera/datasciencecoursera/datasciencecoursera')
  #directory <- "specdata"
  #id=1:3
  
  setwd(directory)
  files <- list.files()
  complete_polut <- data.frame(matrix(nrow=length(id),ncol=2))
  colnames(complete_polut) <- c('id','nobs')
  for (i in 1:length(id)){
    data <- read.csv(files[id[i]])
    complete_polut$id[i] <- data$ID[1]
    complete_polut$nobs[i] <- sum(complete.cases(data))
  }
  return(complete_polut)
}

setwd('/Users/nmouquet/Documents/Math-Prog-Stat/R/coursera/datasciencecoursera/datasciencecoursera')
complete("specdata",54)

set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])


corr <- function(directory,threshold=0) {
  
  setwd(directory)
  files <- list.files(pattern =".csv")
  cor_polut <- vector()
  pos=0
  for (i in 1:length(files)){
    data <- read.csv(files[i])
    if (sum(complete.cases(data))>threshold){
      pos=pos+1
      data <- na.omit(data)
      cor_polut[pos]=cor(data$sulfate,data$nitrate)
    }
  }
  return(cor_polut)
}

setwd('/Users/nmouquet/Documents/Math-Prog-Stat/R/coursera/datasciencecoursera/datasciencecoursera')
cr <- corr("specdata")
head(cr)
summary(cr)
length(cr)

setwd('/Users/nmouquet/Documents/Math-Prog-Stat/R/coursera/datasciencecoursera/datasciencecoursera')

cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)                
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)
