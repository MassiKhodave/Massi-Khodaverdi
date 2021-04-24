#-----------------------------------------
# FUNCTION SimulateData
# description: Simulating flowering-time variables from two normal distributions corresponding to control (no-vernalization) and treatment (vernalized) groups
# inputs: Mean and SD for each group
#         sample size (per group)
# outputs: a dataframe of simulated data
##############################
SimulateData <- function(noVernMean=60, noVernSD=20, vernMean=30, vernSD=7, sampleSize=100) {

  noVern <- rnorm(n=100,mean=60,sd=20)
  vern <- rnorm(n=100,mean=30,sd=7)

  # creating a dataframe from the two group
  data <- c(noVern,vern)
  treatment <- c(rep("Control", length(noVern)), rep("Vern", length(vern)))
  df <- data.frame(1:length(data),treatment,data)
  names(df) <- list("ID", "Treatment", "TimeToFlower")
  
  return(df)
}# end of SimulateData
#-----------------------------------------

df <- SimulateData()

#-----------------------------------------
# FUNCTION RunANOVA
# description: Analyze a given data by means of ANOVA
# inputs: a dataframe containing data samples
# outputs: Resulting output of ANOVA run over the input
##############################
RunANOVA <- function(df=NULL) {
  anova <- aov(TimeToFlower~Treatment, data=df)
  return(summary(anova))
}# end of RunANOVA
#-----------------------------------------

RunANOVA(df)

#-----------------------------------------
# FUNCTION BoxPlot
# description: Create a box-plot of samples in two (or more) groups in a given dataframe
# inputs: a dataframe containing data samples
# outputs: Displaying the box-plot
##############################
BoxPlot <- function(df=NULL) {
  ANOplot <- ggplot(data=df, aes(x=Treatment, y=TimeToFlower, fill = Treatment)) + geom_boxplot()
  print(ANOplot)
}# end of BoxPlot
#-----------------------------------------

BoxPlot(df)


#-----------------------------------------
# FUNCTION LinReg
# description: Restructure the simulated data and perform a linear regression over Time-To-Flowering (TTF) variable across control and vernalized groups
# inputs: a dataframe containing data samples
# outputs: Linear regression parameters and its plot
##############################
LinReg <- function(df=NULL) {
  Vern_TTF <- df[df$Treatment=='Vern',]$TimeToFlower
  NoVern_TTF <- df[df$Treatment=='Control',]$TimeToFlower
  new_df <- data.frame(1:length(Vern_TTF),Vern_TTF,NoVern_TTF)
  names(new_df) <- list("ID", "Vern_TTF", "NoVern_TTF")
  
  LinRegPars <- lm(Vern_TTF~NoVern_TTF, data=new_df)
  
  LinRegPlot <- ggplot(new_df)+aes(x=Vern_TTF,y=NoVern_TTF)+geom_point()+stat_smooth(method=lm,se=0.99)
  
  print(LinRegPlot)
  
  return(LinRegPars)
}# end of LinReg
#-----------------------------------------