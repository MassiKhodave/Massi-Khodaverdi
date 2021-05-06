#-----------------------------------------
# FUNCTION ZeroCounter
# description: Counting the number of zeros in an input vector with a for-loop
# inputs: a vector (matrix)
# outputs: number of zeros in the vector
##############################
ZeroCounter <- function(vec=NULL) {
  if(is.null(vec)){
    vec <- c(rep(0,3),rep(1,3),rep(2,3))
  }
  
  counter <- 0
  
  for(e in vec){
    if (e==0){
      counter = counter + 1
    }
  }
  
  return(counter)
}# end of ZeroCounter
#-----------------------------------------


#-----------------------------------------
# FUNCTION SingleLine_ZeroCounter
# description: Counting the number of zeros in an input vector with subsetting
# inputs: a vector (matrix)
# outputs: number of zeros in the vector
##############################
SingleLine_ZeroCounter <- function(vec=NULL) {
  
  if(is.null(vec)){
    vec <- c(rep(0,3),rep(1,3),rep(2,3))
  }
  
  zcount <- length(vec[vec==0])
  
  return(zcount)
  
}# end of SingleLine_ZeroCounter
#-----------------------------------------


#-----------------------------------------
# FUNCTION FormMatrix
# description: Creating a matrix with a specified shape, in which the value at element (i,j) is ixj
# inputs: number of columns and rows
# outputs: creating a matrix with element (i,j) having values ixj
##############################
FormMatrix <- function(nrow=3,ncol=3) {
  
  M <- matrix(nrow=nrow, ncol=ncol)
  for (i in 1:nrow){
    for (j in 1:ncol){
      M[i,j] = i*j
    }
  }
  
  return(M)
}# end of FormMatrix
#-----------------------------------------

SimulateData <- function() {
  
  noVern <- rnorm(n=10,mean=65,sd=10)
  vern2 <- rnorm(n=10,mean=50,sd=7)
  vern4 <- rnorm(n=10,mean=40,sd=5)
  vern6 <- rnorm(n=10,mean=35,sd=5)
  vern8 <- rnorm(n=10,mean=32,sd=3)
  
  # creating a dataframe from the two group
  data <- c(noVern,vern2,vern4,vern6,vern8)
  treatment <- c(rep(0, length(noVern)), rep(2, length(vern2)), rep(4, length(vern4)), rep(6, length(vern6)), rep(8, length(vern8)))
  
  df <- data.frame(1:length(data),treatment,data)
  names(df) <- list("ID", "VernDuration", "TimeToFlower")
  
  return(df)
}


###################################################
# function: readData
# read in (or generate) data set for analysis
# input: file name (or nothing, for this demo)
# output: 3 column data frame of observed data (ID,x,y)
#------------------------------------------------- 
readData <- function(z=NULL) {
  if(is.null(z)){
    xObs <- 1:20
    yObs <- xObs + 10*rnorm(20)
    dF <- data.frame(ID=seq_along(xObs),xObs,yObs)} # set up data frame
  
  dF <-read.table(file=z,
                  header=TRUE,
                  sep=",",
                  stringsAsFactors=FALSE)
  return(dF)
}# end of readData
#-----------------------------------------

##################################################
# function: getMetric
# calculate metric for randomization test
# input: 2-column data frame for regression
# output: ANOVA P-value
#------------------------------------------------- 
getMetric <- function(z=NULL) {
  if(is.null(z)){
    xObs <- 1:20
    yObs <-  xObs + 10*rnorm(20)
    z <- data.frame(ID=seq_along(xObs),xObs,yObs)} # set up data frame                 
  
  . <- lm(z[,3]~z[,2])
  . <- summary(.)
  . <- .$coefficients[2,1]
  
  slope <- .
  return(slope)
}# end of getMetric
#-----------------------------------------

##################################################
# function: shuffleData
# randomize data for regression analysis
# input: 3-column data frame (ID,xVar,yVar)
# output: 3-column data frame (ID,xVar,yVar)
#------------------------------------------------- 
shuffleData <- function(z=NULL) {
  if(is.null(z)){
    xObs <- 1:20
    yObs <- xObs + 3*rnorm(20)
    z <- data.frame(ID=seq_along(xObs),xObs,yObs)} # set up data frame                 
  z[,3] <- sample(z[,3]) # use sample function with defaults to reshuffle column
  
  return(z)
}# end of shuffleData
#-----------------------------------------


##################################################
# function: getPVal
# calculate p value from simulation
# input: list of observed metric, and vector of simulated metrics
# output: lower, upper tail probability values
#------------------------------------------------- 
getPVal <- function(z=NULL) {
  if(is.null(z)){
    z <- list(xObs=runif(1),xSim=runif(1000))}
  pLower <- mean(z[[2]]<=z[[1]])
  pUpper <- mean(z[[2]]>=z[[1]])
  return(c(pL=pLower,pU=pUpper))
}# end of getPVal
#-----------------------------------------


##################################################
# function: plotRanTest
# create ggplot of histogram of simulated values
# input: list of observed metric and vector of simulated metrics
# output: saved ggplot graph
#------------------------------------------------- 
plotRanTest <- function(z=NULL) {
  if(is.null(z)){
    z <- list(rnorm(1),rnorm(1000)) }
  dF <- data.frame(ID=seq_along(z[[2]]),simX=z[[2]])
  p1 <- ggplot(data=dF,mapping=aes(x=simX))
  p1 + geom_histogram(mapping=aes(fill=I("goldenrod"),color=I("black"))) +
    geom_vline(aes(xintercept=z[[1]],col="blue")) 
  
}
