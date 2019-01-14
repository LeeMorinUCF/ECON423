
################################################################################
# 
# ECON 423 Topics in Financial Economics
# Linear clones of the stock index
# Author: Lee Morin, PhD
# Original version: Jan. 23, 2012
# Last modified: March 19, 2017
# 
# ECON423LinearClones.R gives a sample program for testing the potential to mimic
# the returns on a market index with linear clones.
# 
# Input: dataset like the sample myMutualFundData.csv
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

# Load the data from the file "myMutualFundData.csv".
my_data = read.csv("myStockReturnData.csv")


# Initial inspection of dataset.
nObs <- nrow(my_data)
nObs

colnames(my_data)

summary(my_data)
# Note that returns are measured in percent, ie: 3.907 is 3.907%.


#--------------------------------------------------------------------------------
# Create indicators for in-sample and out-of-sample periods.
#--------------------------------------------------------------------------------

in_sample <- seq(1, 3488)
# Verify dates:
head(my_data[in_sample, 'date']) # Starts at the top.
tail(my_data[in_sample, 'date']) # Ends at end of 2006.

out_sample <- seq(3489, nrow(my_data))
# Verify dates:
head(my_data[out_sample, 'date']) # Starts at the beginning of 2007.
tail(my_data[out_sample, 'date']) # Ends at bottom of dataset.


################################################################################
# Estimation of linear clone.
################################################################################

#--------------------------------------------------------------------------------
# Part (a): Initial estimate with all stocks.
#--------------------------------------------------------------------------------

fmla <- as.formula(rm ~ ABC + DEF + GHI + JKL + MNO + PQR + STU + VWX + XYZ + ZAB)
fmla

# Estimate a linear model (linear regression).
clone_model_1 <- lm(formula = fmla, data = my_data[in_sample, ])

summary(clone_model_1)


#--------------------------------------------------------------------------------
# Part (b): Revised estimates with selected stocks.
#--------------------------------------------------------------------------------

# In this case, all are significant.
# The model stays the same as above.

fmla <- as.formula(rm ~ ABC + DEF + GHI + JKL + MNO + PQR + STU + VWX + XYZ + ZAB)
fmla

# Estimate a linear model (linear regression).
clone_model_2 <- lm(formula = fmla, data = my_data[in_sample, ])

summary(clone_model_2)


# Predict for the whole sample.
my_data[, 'clone'] <- predict(clone_model_2, newdata = my_data)


#--------------------------------------------------------------------------------
# Part (c): Analysis of predictions of returns.
#--------------------------------------------------------------------------------

# Plot the out-of-sample predictions on the actual returns.
plot(my_data[out_sample, 'clone'], my_data[out_sample, 'rm'])


# Correlation coefficient.
cor(my_data[out_sample, 'clone'], my_data[out_sample, 'rm'])


#--------------------------------------------------------------------------------
# Part (d): Analysis of predictions of returns.
#--------------------------------------------------------------------------------

# Generate cumulative returns, starting with $1 invested at beginning of out-of-sample period.
# Start by adding the returns.
my_data[, 'clone_value'] <- exp(cumsum(my_data[, 'clone']/100))
# Normalize by the value on the first day of 2007.
my_data[, 'clone_value'] <- my_data[, 'clone_value']/my_data[out_sample[1], 'clone_value']

# Repeat for the index.
my_data[, 'mkt_value'] <- exp(cumsum(my_data[, 'rm']/100))
my_data[, 'mkt_value'] <- my_data[, 'mkt_value']/my_data[out_sample[1], 'mkt_value']


# Plot them together over the entire sample.
plot(1:nrow(my_data), my_data[, 'mkt_value'], col = 'red',
     main = c('Market Index vs. Linear Clone', '(Entire sample)'), 
     xlab = 'Time, in Days', ylab = 'Value')
points(1:nrow(my_data), my_data[, 'clone_value'], col = 'blue')


# Plot them together in the out-of-sample period.
plot(1:length(out_sample), my_data[out_sample, 'mkt_value'], col = 'red',
     main = c('Market Index vs. Linear Clone', '(Out of Sample)'), 
     xlab = 'Time, in Days', ylab = 'Value')
points(1:length(out_sample), my_data[out_sample, 'clone_value'], col = 'blue')


#--------------------------------------------------------------------------------
# Part (e): Regression out-of-sample.
#--------------------------------------------------------------------------------

# Estimate the same model as above in the out-of-sample period.
fmla <- as.formula(rm ~ ABC + DEF + GHI + JKL + MNO + PQR + STU + VWX + XYZ + ZAB)
fmla

# Estimate a linear model (linear regression).
clone_model_3 <- lm(formula = fmla, data = my_data[in_sample, ])

summary(clone_model_3)
# Did the weights change very much?
# How does this relate to the results in parts (c) and (d)?




################################################################################
# End.
################################################################################
