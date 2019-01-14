
################################################################################
# 
# ECON 423 Topics in Financial Economics
# Analysis of hypothetical hedge fund returns
# Author: Lee Morin, PhD
# Original version: Jan. 23, 2012
# Last modified: March 19, 2017
# 
# ECON423HedgeFunds.R gives a sample program for analyzing returns of hypothetical hedge funds.
# The hypothetical hedge funds are based on investments in the S&P 500 index 
# and in call and put options on the index.
# 
# Input: dataset like the sample SPXandVIXdata.csv
# Output: Prints and saves estimation output.
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
# setwd("C:/Documents and Settings/path/to/your/folder")
setwd("C:/Users/iky155/Documents/RandD/ECON423/ECON423w2017/ECON423A5")     

# Load the data from the file "SPXandVIXdata.csv".
my_data = read.csv("SPXandVIXdata.csv")


# Initial inspection of dataset.
nObs <- nrow(my_data)
nObs

colnames(my_data)

summary(my_data)

# Series in the dataset are: 
# date: date at beginning of each month
# spx: spx index level at beginning of each month
# spxt: spx level at contract expiry (15 trading days later, when payoffs are determined)
# vix: vix index level at beginning of month (a proxy for the volatility parameter)




################################################################################
# Calculate hypothetical option prices.
################################################################################


#--------------------------------------------------------------------------------
# Set parameters.
#--------------------------------------------------------------------------------

# Fix the risk-free rate at 2%.
r = 0.02


# Choose the strike price of the option by choosing k.
# The strike price K will be k % above the level of the index S, that is, K = (1+k)*S. 

# For k = 0, the options are at-the-money, i.e. K = S.
k = 0.0 

# Generate the strike price, relative to the current index level.
my_data[, 'K'] <- my_data[, 'spx']*(1 + k)
summary(my_data)


#--------------------------------------------------------------------------------
# Calculate the inputs to the Black-Scholes formula.
#--------------------------------------------------------------------------------


my_data[, 'd1'] <- 
  ( log(my_data[, 'spx']/my_data[, 'K']) + (r + (my_data[, 'vix']/100)^2/2)*sqrt(15/252) ) / 
  ( my_data[, 'vix']/100*sqrt(15/252) )

my_data[, 'd2'] <- my_data[, 'd1'] - my_data[, 'vix']/100*sqrt(15/252)


summary(my_data)


#--------------------------------------------------------------------------------
# Calculate the option prices using the Black-Scholes formula.
#--------------------------------------------------------------------------------


# Calculate call option prices.
my_data[, 'call'] <- 
  pnorm(my_data[, 'd1']) * my_data[, 'spx'] - 
  pnorm(my_data[, 'd2']) * my_data[, 'K'] * exp( - r*15/252)



# Calculate put option prices.
my_data[, 'put'] <- 
  pnorm( - my_data[, 'd2']) * my_data[, 'K'] * exp( - r*15/252) - 
  pnorm( - my_data[, 'd1']) * my_data[, 'spx']


summary(my_data)


################################################################################
# Calculate payoffs and returns on assets.
################################################################################


# Returns on the underlying index.
my_data[, 'spx_ret'] <-  my_data[, 'spxT']/my_data[, 'spx'] - 1


# Calculate payoffs and returns on call options.
my_data[, 'call_pay'] <- pmax( my_data[, 'spxT'] - my_data[, 'K'], 0 )
my_data[, 'call_ret'] <- my_data[, 'call_pay']/my_data[, 'call'] - 1


# Calculate payoffs and returns on put options.
my_data[, 'put_pay'] <- pmax( my_data[, 'K'] - my_data[, 'spxT'], 0 )
my_data[, 'put_ret'] <- my_data[, 'put_pay']/my_data[, 'put'] - 1


summary(my_data)


################################################################################
# Calculate returns on trading strategies.
################################################################################




#--------------------------------------------------------------------------------
# Portfolio Insurance.
#--------------------------------------------------------------------------------

# Buy the index and also buy put options to insure against negative returns.
# Number of units n_PI invested in the index, each index unit is combined with a put option.
# Total cost of n_PI*spx and n_PI*put at beginning of month is equal to one unit of wealth.

my_data[, 'n_PI'] <- 1/( my_data[, 'spx'] + my_data[, 'put'] )
my_data[, 'PI_ret'] <- my_data[, 'n_PI'] * (my_data[, 'put_pay'] + my_data[, 'spxT']) - 1

summary(my_data)


#--------------------------------------------------------------------------------
# Covered Calls.
#--------------------------------------------------------------------------------

# Buy the Index and short call options on the index.
# Number of units n_CC invested in the index, each index unit is combined with a short call option.
# Total cost of n_CC*spx and 0.5*n_CC*call at beginning of month is equal to one unit of wealth.
# Note: This allows for 50% margin on the covered call.

my_data[, 'n_CC'] <- 1/( my_data[, 'spx'] + 0.5*my_data[, 'call'] )
my_data[, 'CC_ret'] <- my_data[, 'n_CC'] * ( my_data[, 'spxT'] - my_data[, 'call_pay'] + 
                                              1.5*my_data[, 'call']*(1 + r*15/252) ) - 1
# Note: Interest is earned on call option premium and margin requirement.

summary(my_data)



#--------------------------------------------------------------------------------
# Capital Addition Partners.
#--------------------------------------------------------------------------------

# Invest a portion of the fund in call options and keep the rest in risk-free bonds.
# Weight w_CAP invested in call options, the rest in risk-free bonds.


w_CAP <- 0.25
my_data[, 'CAP_ret'] <- w_CAP*my_data[, 'call_ret'] + (1 - w_CAP)*r*15/252

summary(my_data)


#--------------------------------------------------------------------------------
# Capital Decimation Partners.
#--------------------------------------------------------------------------------

# Short put options and hold the premiums and additional margin in risk-free bonds.
# Number of units n_CDP short puts, premium and margin invested risk-free.
# Number of units n_CDP is set so premia collected is worth 25% of wealth.

w_CDP = 0.25
my_data[, 'n_CDP'] <- w_CDP/my_data[, 'put']
my_data[, 'CDP_ret'] <- (1 + w_CDP)*(1 + r*15/252) - my_data[, 'n_CDP']*my_data[, 'put_pay'] - 1

summary(my_data)

# Note: Interest is earned on put option premium and margin requirement.
my_data[, 'CDP_ret'] <- pmax( my_data[, 'CDP_ret'], -1 )

summary(my_data)
# Note: Lowest return is a total loss of capital (this strategy can do MUCH worse).




################################################################################
# Analysis of monthly returns on investment strategies.
################################################################################

#--------------------------------------------------------------------------------
# Plot returns on investment strategies.
#--------------------------------------------------------------------------------

# Plot returns on SPX index.
plot(my_data[, 'spxT'] / my_data[, 'K'], my_data[, 'spx_ret'],
     main = 'Return on S&P 500', 
     xlab = 'Index Level / Strike Price', ylab = 'Return')
# Of course, SPX returns are a linear function of SPX returns.

# Now consider call and put options.
plot(my_data[, 'spxT'] / my_data[, 'K'], my_data[, 'call_ret'],
     main = 'Return on Call Options on S&P 500', 
     xlab = 'Index Level / Strike Price', ylab = 'Return')


plot(my_data[, 'spxT'] / my_data[, 'K'], my_data[, 'put_ret'],
     main = 'Return on Put Options on S&P 500', 
     xlab = 'Index Level / Strike Price', ylab = 'Return')




#--------------------------------------------------------------------------------
# Calculate summary statistics for monthly returns.
#--------------------------------------------------------------------------------

# Initiate with the index, calls and puts.
summary(my_data[, c('spx_ret', 'call_ret', 'put_ret')])

# Calculate individual statistics.
colMeans(my_data[, c('spx_ret', 'call_ret', 'put_ret')])

# Standard deviations can be calculated from the diagonal of the covariance matrix...
sqrt(diag(var(my_data[, c('spx_ret', 'call_ret', 'put_ret')])))

# ... or column-by-column using the apply function.
apply(my_data[, c('spx_ret', 'call_ret', 'put_ret')], 
      2, function(x) var(x)) 
# This calculates the var() function on the columns (dimension 2) of the matrix provided.

# Similarly for the 5% and 95% quantiles.
apply(my_data[, c('spx_ret', 'call_ret', 'put_ret')], 
      2, function(x) quantile(x, c(0.05, 0.95))) 









################################################################################
# Generate and summarize 3-month returns.
################################################################################

# SPX index.
my_data[3:nrow(my_data), 'spx_ret3'] <- 
  (1 + my_data[1 : (nrow(my_data) - 2), 'spx_ret']) * 
  (1 + my_data[2 : (nrow(my_data) - 1), 'spx_ret']) * 
  (1 + my_data[3 : (nrow(my_data) - 0), 'spx_ret']) - 1

# Portfolio Insurance.
my_data[3:nrow(my_data), 'PI_ret3'] <- 
  (1 + my_data[1 : (nrow(my_data) - 2), 'PI_ret']) * 
  (1 + my_data[2 : (nrow(my_data) - 1), 'PI_ret']) * 
  (1 + my_data[3 : (nrow(my_data) - 0), 'PI_ret']) - 1

# Covered Calls.
my_data[3:nrow(my_data), 'CC_ret3'] <- 
  (1 + my_data[1 : (nrow(my_data) - 2), 'CC_ret']) * 
  (1 + my_data[2 : (nrow(my_data) - 1), 'CC_ret']) * 
  (1 + my_data[3 : (nrow(my_data) - 0), 'CC_ret']) - 1

# Capital Addition Partners.
my_data[3:nrow(my_data), 'CAP_ret3'] <- 
  (1 + my_data[1 : (nrow(my_data) - 2), 'CAP_ret']) * 
  (1 + my_data[2 : (nrow(my_data) - 1), 'CAP_ret']) * 
  (1 + my_data[3 : (nrow(my_data) - 0), 'CAP_ret']) - 1

# Capital Decimation Partners.
my_data[3:nrow(my_data), 'CDP_ret3'] <- 
  (1 + my_data[1 : (nrow(my_data) - 2), 'CDP_ret']) * 
  (1 + my_data[2 : (nrow(my_data) - 1), 'CDP_ret']) * 
  (1 + my_data[3 : (nrow(my_data) - 0), 'CDP_ret']) - 1


summary(my_data)


################################################################################
# Calculate summary statistics, just as was done above for monthly returns.
################################################################################

# Copy code from above and modify for 3-month returns.


colMeans(my_data[3:nrow(my_data), c('spx_ret3', 'PI_ret3', 'CC_ret3', 'CAP_ret3', 'CDP_ret3')])

colMeans(my_data[, c('spx_ret3', 'PI_ret3', 'CC_ret3', 'CAP_ret3', 'CDP_ret3')], na.rm = TRUE)




################################################################################
# End.
################################################################################


