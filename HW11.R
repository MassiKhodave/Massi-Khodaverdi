library(MASS)

z <- read.table("Lengths.csv",header=TRUE,sep=",", stringsAsFactors=FALSE)

# fitting Gamma distribution
gammaPars <- fitdistr(z$Lengths,"gamma")
shapeML <- gammaPars$estimate["shape"]
rateML <- gammaPars$estimate["rate"]

# simulating data
nSim = 100
n <- length(z[,1])
for (i in 1:nSim){
  simz <- rgamma(n=n, shape=shapeML, rat=rateML)
  simz <- data.frame(1:n, simz)
  names(simz) <- list("ID", "length")
  write.csv(simz, paste("LengthData/","SimData_",i,".csv",sep=""), row.names=FALSE)
}


##################################################
# function: regStats
# fits linear model, extracts statistics
# input: 2-column data frame (x and y)
# output: slope, p-value, and r2
#------------------------------------------------- 
regStats <- function(d=NULL) {
  if(is.null(d)) {
    xVar <- runif(10)
    yVar <- runif(10)
    d <- data.frame(xVar,yVar)
  }
  . <- lm(data=d,d[,2]~d[,1])
  . <- summary(.)
  statsList <- list(Slope=.$coefficients[2,1],
                    pVal=.$coefficients[2,4],
                    r2=.$r.squared)
  return(statsList)
  
}

#--------------------------------------------
# Global variables
fileFolder <- "LengthData/"
fileNames <- list.files(path=fileFolder)
nFiles <- length(fileNames)
fileOut <- "StatsSummary.csv"
#--------------------------------------------

# Create data frame to hold file summary statistics
ID <- seq_along(fileNames)
fileName <- fileNames
slope <- rep(NA,nFiles)
pVal <- rep(NA,nFiles)
r2 <- rep(NA,nFiles)

statsOut <- data.frame(ID,fileName,slope,pVal,r2)

# batch process by looping through individual files
for (i in seq_along(fileNames)) {
  data <- read.table(file=paste(fileFolder,fileNames[i],sep=""),
                     sep=",",
                     header=TRUE) # read in next data file
  
  dClean <- data[complete.cases(data),] # get clean cases
  
  . <- regStats(dClean) # pull regression stats from clean file
  statsOut[i,3:5] <- unlist(.) # unlist, copy into last 3 columns
  
}


# set up output file and incorporate time stamp and minimal metadata
write.table(cat("# Summary stats for ",
                "batch processing of regression models","\n",
                "# timestamp: ",as.character(Sys.time()),"\n",
                "# NJG","\n",
                "# ------------------------", "\n",
                "\n",
                file=fileOut,
                row.names="",
                col.names="",
                sep=""))

# now add the data frame
write.table(x=statsOut,
            file=fileOut,
            row.names=FALSE,
            col.names=TRUE,
            sep=",",
            append=TRUE)
