################################################################################
# 
# ECON 423 Assignment 2 Sample Program
# Mean-Variance Efficient Portfolios
# Lee Morin, PhD
# Department of Economics, Queen's University
# Original version: Jan. 13, 2012
# Last modified: Jan. 22, 2017
# 
# ECON423PortfolioExample.R gives a sample program for the calculation of the 
# portfolio frontier for a number of cases with an arbitrary number of assets.
# It is provided as an example for the solution of ECON423 Assignment 2, Question 1.
# Adapted from a program written by Pierre Chausse, University of Waterloo. 
# 
# Input: data(Finance) from library(gmm).
# Output: Prints data and plots figures.
# Dependencies: library(gmm), library(timeSeries), library(fPortfolio).
# 
################################################################################


################################################################################
# Load required libraries.
library(timeSeries)
library(fPortfolio)
################################################################################
# 
# Note: If R gives the following warning:
# 	Warning messages:
# 	1: package 'fPortfolio' was built under R version 2.14.1 
# 	2: package 'timeSeries' was built under R version 2.14.1 
#		...
# Then the import was successful.
# Otherwise, if R gives the following error message instead:
# 	Error in library(fPortfolio) : there is no package called 'fPortfolio'
# Then you need to add this package to the C:\Program Files\R\R-2.13.0\library folder.
# 
# Manual way:
# Google "CRAN - Package fPortfolio" and download the windows binary file "fPortfolio_2130.80.zip"
# Then open the .zip file and save it in C:\Program Files\R\R-2.13.0\library folder.
# You will then probably have to do the same for other missing packages
# that are required by fPortfolio. 
# These links should be near the top of the fPortfolio page beside the Depends.
# 
# Another way to install a package at the command line is:
# by running the next commands (delete #-sign & press ctrl+R on this line):
# 	install.packages("fPortfolio", repos="http://R-Forge.R-project.org")
# 	install.packages("timeSeries", repos="http://R-Forge.R-project.org")
# 	install.packages("robustbase", repos="http://R-Forge.R-project.org")
# 
################################################################################


################################################################################
# Load the data that you created in a spreadsheet and saved in .csv format.
################################################################################

# Set the working directory to the folder containing the file "mystockprices".
# setwd("C:/Documents and Settings/Students/Desktop/ECON423A2")
setwd("C:/Users/iky155/Documents/RandD/ECON423/ECON423w2017/PortfolioTheory")     

# Load the data from the file "mystockprices.csv".
mydata = read.csv("mystockprices.csv")



# Put data in the expected format for a time series.
# Requires timeSeries library, loaded above.
mydata <- as.timeSeries(mydata)


# List the ticker symbols of the stocks that are included.
colnames(mydata) 

summary(mydata)


################################################################################
# Data manipulation.
################################################################################

# Calculate returns.
returns <- diff(log(mydata))*100
returns <- na.omit(returns)

# If file already in returns.
returns <- mydata


# Separate data into returns and market index.
rM <- returns[,11]
returns <- returns[,1:10]

# List the ticker symbols of the stocks that are included (to verify selection).
colnames(returns) 


# Plot the return series to make sure they look reasonable.
plot(returns, type = "l", xlab = "Date", ylab = "Return", main = "Returns on 10 Stocks")


################################################################################
# Data analysis.
################################################################################


# Now restrict in-sample period to before the financial crisis.
# The rest of the observations are out-of-sample.
# In this dataset, observation 3489 is the first trading day in 2007.
rIn <- returns[ 1:3488, ]
rOut <- returns[ 3489:4012, ]
rMIn <- rM[ 1:3488, ]
rMOut <- rM[ 3489:4012, ]




# Calculate mean vector and variance matrix for in-sample stock return data.
mu_In <- colMeans(rIn)
mu_In
sigma_In <- sqrt(diag(var(rIn)))
sigma_In
rho_In <- cor(rIn)
rho_In

# Calculate mean vector and variance matrix for in-sample index return data.
mu_M_In <- colMeans(rMIn)
mu_M_In
sigma_M_In <- sqrt(diag(var(rMIn)))
sigma_M_In


# This can easily be modified for the out-of-sample data.


################################################################################
# Run this command to compute the features of this portfolio frontier.
# Requires fPortfolio library, lodaed above.
################################################################################

FIn <- portfolioFrontier(rIn, spec = portfolioSpec(portfolio = list(riskFreeRate = 0.007)))
# NOTE: To compute the portfolio frontier, you need to specify the risk-free rate, 
# 	which is zero by default. I used 0.007 here.



# Plot efficient frontier with additional features for the graph.
frontierPlot(FIn, pch = 19)
# Add gridlines and axes for clarity.
grid()
abline(h = 0, col = "grey")
abline(v = 0, col = "grey")
# Plot the portfolio with minimum variance.
minvariancePoints(FIn, pch = 19, col = "red")
# Plot the tangency portfolio. 
tangencyPoints(FIn, pch = 19, col = "blue")
tangencyLines(FIn, col = "blue")
# Compare with the portfolio with equal weights.
equalWeightsPoints(FIn, pch = 15, col = "grey")
# Show the individual assets.
singleAssetPoints(FIn, pch = 19, cex = 1.5, col = topo.colors(6))
# Show the possibilities from all two-asset portfolios.
twoAssetsLines(FIn, lty = 3, col = "black")
# Show the market index.
arrows(0.005,0,sigma_M_In, mu_M_In)
text(0.003,0, "r_M")
# Label the individual assets.
for(i in 1:10) text(sigma_In[i],mu_In[i],names(returns)[i])



################################################################################
# Summarizing the tangency portfolios
################################################################################


# Obtain summary of information about the tangency portfolio:
tangencyPortfolio(rIn, spec = portfolioSpec(portfolio = list(riskFreeRate = 0.007)))


# Optimal portfolio weights:
resIn <- tangencyPortfolio(rIn, spec = portfolioSpec(portfolio = list(riskFreeRate = 0.007)))
wIn <- resIn@portfolio@portfolio$weights
wIn
# Weights need to be in matrix format for matrix multiplication.
wInVec <- as.matrix(wIn)


# Generate the portfolio returns invested over the out-of-sample period.
# Note: t(A) is the transpose and %*% denotes matrix multiplication.
rOut_T <- rOut %*% wInVec



# Plot the series together to compare our efficient portfolio with the market.
plot(rOut_T, rMOut, type="p", col="blue", 
     xlab="r_T", ylab="r_M", main="Out-of-sample Returns: r_T vs. r_M")
grid()



################################################################################
# For Assignment 2, Question 1:
# Based on the above calculations, address the questions posed in the assignment.
# For some questions, you will have to copy and modify sections of the program, 
#   such as changing in-sample to out-of-sample data.
# You can copy and paste directly from the console window for your write-up.
################################################################################



################################################################################
# End
################################################################################
