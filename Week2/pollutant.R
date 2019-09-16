
#program - 1 
pollutantmean <- function(directory, pollutant, id = 1:332)
{
  files_list <- dir(directory, full.names = TRUE)
  data <- data.frame() #Empty data frame
  for(i in id)
  {
    data <- rbind(data, read.csv(files_list[i])) #Bind CSV files in data     
  }

  mean(data[, pollutant],na.rm=TRUE)
  
}

print(pollutantmean("E:/R_Course/datascience_coursera/week1/specdata/", "sulfate", 1:10))
print(pollutantmean("E:/R_Course/datascience_coursera/week1/specdata/", "nitrate", 70:72))
print(pollutantmean("E:/R_Course/datascience_coursera/week1/specdata/", "sulfate", 34))
print(pollutantmean("E:/R_Course/datascience_coursera/week1/specdata/", "nitrate"))

#Program - 2
complete <- function(directory, id)
{
 nobs <- c()
 for(i in id)
 {
   nobs <- c(nobs, nrow(na.omit(read.csv(paste(directory, sprintf("%03d", i), '.csv', sep="")))))
 }
 data.frame('id' = id, 'nobs' = nobs)
}

cc <- complete("E:/R_Course/datascience_coursera/week1/specdata/", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)

cc <- complete("E:/R_Course/datascience_coursera/week1/specdata/", 54)
print(cc$nobs)

RNGversion("3.5.1")  
set.seed(42)
cc <- complete("E:/R_Course/datascience_coursera/week1/specdata/", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])



#program - 3
corr <- function(directory, threshold = 0)
{
  result <- c()
  for(f in dir(directory))
  {
    df <- na.omit(read.csv(paste(directory,f, sep="")))
    if(nrow(df) > threshold)
    {
      result <- c(result, cor( df$sulfate, df$nitrate))
    }
  }
  result
}

cr <- corr("E:/R_Course/datascience_coursera/week1/specdata/")                
cr <- sort(cr)   
RNGversion("3.5.1")
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- corr("E:/R_Course/datascience_coursera/week1/specdata/", 129)                
cr <- sort(cr)                
n <- length(cr)    
RNGversion("3.5.1")
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

cr <- corr("speE:/R_Course/datascience_coursera/week1/specdata/", 2000)                
n <- length(cr)                
cr <- corr("E:/R_Course/datascience_coursera/week1/specdata/", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))