################################################################################
# 
# ECON 423 Sample Program
# Portfolio Theory with N Assets
# Lee Morin, PhD
# Department of Economics, Queen's University
# Original version: Jan. 13, 2012
# Last modified: Jan. 22, 2017
# 
# ECON423PortfolioNassets.R gives some examples of the calculation of the 
# portfolio frontier for a number of cases with an arbitrary number of assets.
# Adapted from a program written by Pierre Chausse. 
# 
# Input: data(Finance) from library(gmm).
# Output: Prints data and plots figures.
# Dependencies: library(gmm), library(timeSeries), library(fPortfolio).
# 
################################################################################


################################################################################
# Load the library gmm because it contains the dataset 'Finance'.
# Finance contains 20 daily stock return series over the period
# 	Jan. 5, 1993 to Jan. 30, 2010 (4012 observations).
# Observation 3489 is Jan. 3, 2007.
################################################################################

library(gmm)
data(Finance)

# Inspect contents of this dataset.
colnames(Finance)


################################################################################
# Choose the set of stocks to include in the portfolio.
################################################################################

# Examples:
# Select first 10 stocks.
# r <- Finance[,1:10]
# Select the 10 even numbered stocks.
r <- Finance[, c(2,4,6,8,10,12,14,16,18,20) ]

# Display the ticker symbols for this selection.
colnames(r)

# Change the numbers to reflect a different choice of the stocks described
# 	in the description for the dataset "Finance" on page 8 of the manual gmm.pdf.
# Find the Reference manual gmm.pdf at 
# 	http://cran.r-project.org/web/packages/gmm/index.html
# Google "CRAN package gmm" to find it. 


################################################################################
# Obtain the market return.
################################################################################

rM <- Finance[,22]
rMbar <- mean(rM)
rMvar <- var(rM)


################################################################################
# Put data in the expected format of a time series.
# 	Requires timeSeries library.
################################################################################

library(timeSeries)
r <- as.timeSeries(r) 
rM <- as.timeSeries(rM) 

# List the ticker symbols of the stocks that are included.
colnames(r) 


################################################################################
# Calculate mean vector and variance matrix.
################################################################################

mu <- colMeans(r)
mu
sigma <- var(r)
sigma
rho <- cor(r)
rho


################################################################################
# Run this command and it computes the features of this portfolio frontier.
# 	Requires fPortfolio library.
################################################################################

library(fPortfolio)

F <- portfolioFrontier(r, spec = portfolioSpec(portfolio = list(riskFreeRate = 0.007)))
# NOTE: To compute the portfolio frontier, you need to specify the risk-free rate, 
# 	which is zero by default. I used 0.007 here.


# Plot the frontier:
frontierPlot(F, pch = 19)
# Add gridlines and axes for clarity.
grid() 
abline(h = 0, col = "grey")
abline(v = 0, col = "grey")


# Plot again with additional features for the graph.
frontierPlot(F, pch = 19)
# Add gridlines and axes for clarity.
grid() 
abline(h = 0, col = "grey")
abline(v = 0, col = "grey")
# Plot the portfolio with minimum variance.
minvariancePoints(F, pch = 19, col = "red")
# Plot the tangency portfolio. 
tangencyPoints(F, pch = 19, col = "blue")
tangencyLines(F, col = "blue")
# Compare with the portfolio with equal weights.
equalWeightsPoints(F, pch = 15, col = "grey")
# Show the individual assets.
singleAssetPoints(F, pch = 19, cex = 1.5, col = topo.colors(6))
# Show the possibilities from all two-asset portfolios.
twoAssetsLines(F, lty = 3, col = "grey")



################################################################################
# Compute the frontier using a Monte Carlo method which constructs 
# 	random portfolios from the dataset.
################################################################################

frontierPlot(F, col = c("orange", "orange"), pch = 19)
monteCarloPoints(F, mcSteps = 5000, cex = 0.25, pch = 19)
twoAssetsLines(F, lwd = 2, col = "orange")
abline(v = 0, col = "grey")


################################################################################
# Summarizing the tangency portfolios
################################################################################


# For a summary of information about the tangency portfolio:
tangencyPortfolio(r, spec = portfolioSpec(portfolio = list(riskFreeRate = 0.007)))



# Otherwise, extract the info separately and get the equation for the optimal 
# 	frontier as follows:

# Optimal portfolio weights:
res <- tangencyPortfolio(r, spec = portfolioSpec(portfolio = list(riskFreeRate = 0.007)))
w <- res@portfolio@portfolio$weights
w

# Mean return of tangency portfolio.
mu_T <- res@portfolio@portfolio$targetReturn["mean"]
mu_T

# Standard deviation of tangency portfolio.
sigma_T <- res@portfolio@portfolio$targetRisk["Sigma"]
sigma_T





################################################################################
# End
################################################################################
