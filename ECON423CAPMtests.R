
################################################################################
# 
# ECON 423 Topics in Financial Economics
# Testing the CAPM
# Author: Lee Morin, PhD
# Original version: Jan. 23, 2012
# Last modified: Jan. 22, 2017
# 
# ECON423CAPMtestsR.R gives a sample program for testing the Capital Asset 
#   Pricing Model.
# 
# Input: dataset like the sample myCAPMdata.csv
# Output: Prints data and plots figures.
# Dependencies: None.
# 
################################################################################



################################################################################
# Clear workspace and load data
################################################################################


# Clear workspace.
rm(list=ls(all=TRUE))

# Load the data that you created in a spreadsheet and saved in .csv format.

# Set the working directory to the folder containing the file "mystockprices".
# setwd("C:/Documents and Settings/Students/Desktop/ECON423")     
# Change to your path:
# setwd("C:/path_to_your_CAPM_tests_folder")     
setwd("C:/Users/iky155/Documents/RandD/ECON423/ECON423w2017/TestingCAPM")     

# Load the data from the file "myCAPMdata.csv".
mydata = read.csv("myCAPMdata.csv")
nObs <- nrow(mydata)

colnames(mydata)

################################################################################
# Open a log file to keep track of any results printed to screen.
# Optional. Remove # on sink() command below to uncomment.
# Must follow with an empty sink() command at end to close the file (see bottom).
# Warning: This prints all output to the .log file.
#   Recommend displaying to screen on the first attempt.
# sink("ECON423CAPMtests1.log", append=FALSE, split=FALSE)
################################################################################



################################################################################
# Tests of the CAPM.
################################################################################


#--------------------------------------------------------------------------------
# Generate excess returns on the market index.
#--------------------------------------------------------------------------------

# Note that returns are measured in percent, ie: 1.26 is 1.26%.
# First restate rf as daily percent return (avg. 252 trading days/yr.).
# Make sure the returns are stated in the same units.
mydata[,'rf'] <- mydata[,'rf']/252 # Might not be necessary for your data.
mydata[,'rmXsRet'] <- mydata[,'rm'] - mydata[,'rf']


# Inspect data for consistency.
summary(mydata)


#--------------------------------------------------------------------------------
# Tests of the CAPM for parts (a) and (b).
#--------------------------------------------------------------------------------


# Generate a data frame to store the betas and average excess returns.
# myresults <- data.frame(beta=numeric(10),avgXsRet=numeric(10))
myresults <- data.frame(alpha=numeric(10), tValueAlpha=numeric(10), 
                        beta=numeric(10), tValueBeta=numeric(10), 
                        # Modify as more variables are added.
                        avgXsRet=numeric(10))
rownames(myresults) <- colnames(mydata)[2:11]




# First pass regression using a loop.
for(stockNum in 1:length(rownames(myresults))) {
  
  # Extract ticker symbol for this stock.
  ticker <- rownames(myresults)[stockNum]
  
  print('')
  print(sprintf("Estimating CAPM for %s, stock number %d.",
                ticker,stockNum))
  
  # Generate excess returns from stock prices.
  # Note: Risk free rate rf is already in returns.
  mydata[-1,'XsRet'] <- log(mydata[-1,ticker]/mydata[-nObs,ticker])*100 - mydata[-1,'rf']
  # Note the syntax x[-nObs,'colName] denotes the column 'colName, 
  #   less the obsn numbered nObs (i.e., the last row omitted).
  
  # Specify the equation to estimate.
  fmla <- as.formula('XsRet ~ rmXsRet')
  # Format for equation is:
  # fmla <- as.formula('Y ~ X1 + X2 + X3 ')
  
  # Estimate this linear model.
  lmFits <- lm(formula=fmla,data=mydata)
  
  print(lmFits)
  print(summary(lmFits))
  
  # Record the beta estimate in a matrix.
  myresults[stockNum,'alpha'] <- lmFits$coefficients['(Intercept)']
  myresults[stockNum,'tValueAlpha'] <- coef(summary(lmFits))['(Intercept)','t value']
  myresults[stockNum,'beta'] <- lmFits$coefficients['rmXsRet']
  myresults[stockNum,'tValueBeta'] <- coef(summary(lmFits))['rmXsRet','t value']
  
  
  print(sprintf("Summarizing excess returns for %s, stock number %d.",
                ticker,stockNum))
  
  print(summary(mydata[-1,'XsRet']))
  
  # Record the average excess return in a matrix.
  myresults[stockNum,'avgXsRet'] <- mean(mydata[,'XsRet'],na.rm=TRUE)
  
  
  
}


# Display the results.
print(myresults)


################################################################################
# Analysis and Interpretation
################################################################################



#--------------------------------------------------------------------------------
# Test (a): Fit of the CAPM equation, one security at a time.
#--------------------------------------------------------------------------------


# The CAPM states:  r_i = r_f + beta*(r_m - r_f) + epsilon
# Or:               r_i - r_f = beta*(r_m - r_f) + epsilon
# Translate this into testable hypotheses as you analyze the results.


#--------------------------------------------------------------------------------
# Test (b): Fit of the CAPM equation, all CAPM regressions together.
#--------------------------------------------------------------------------------


# Plot the betas against the average excess returns.
plot(myresults[,'beta'], myresults[,'avgXsRet'],
     type='p',col='blue',
     main=c('Test of the CAPM','Using 10 NYSE stocks, 1993-2009'),
     xlab='Estimated Beta',
     ylab='Average Excess Return',
     sub='Does this support the CAPM?')

# Regression test of the above.
# ...
# See the above code for reference.



# Specify the equation to estimate.
fmla <- as.formula('XsRet ~ rmXsRet')
# Format for equation is:
# fmla <- as.formula('Y ~ X1 + X2 + X3 ')

# Estimate this linear model.
lmFits <- lm(formula=fmla,data=mydata)

print(lmFits)
print(summary(lmFits))




#--------------------------------------------------------------------------------
# Test (c): Fit of the CAPM equation, one security at a time,
#   with additional factors included.
#--------------------------------------------------------------------------------

# Run a modified version of the regressions in part (a).






################################################################################
# Save results in the log file.
# Only required if opening sink() command was used above.
# sink()
################################################################################



################################################################################
# End
################################################################################
