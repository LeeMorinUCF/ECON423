
################################################################################
# 
# ECON 423 Topics in Financial Economics
# Factor models for mutual funds
# Author: Lee Morin, PhD
# Original version: Jan. 23, 2012
# Last modified: March 19, 2017
# 
# ECON423FactorModels.R gives a sample program for testing factor models on mutual fund data.
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
my_data = read.csv("myMutualFundData.csv")
# my_data = read.csv("myMutualFundData.csv", sep = '\t')

# write.csv(my_data, file = 'myMutualFundDataV2.csv', sep = ',')

# Initial inspection of dataset.
nObs <- nrow(my_data)
nObs

colnames(my_data)

summary(my_data)
# Note that returns are measured in percent, ie: 3.907 is 3.907%.

# Extract the list of names of factors.
factor_list <- colnames(my_data)[47 : ncol(my_data)]

# Verify by printing to screen.
factor_list


################################################################################
# Set up the estimation of factor models.
################################################################################


#--------------------------------------------------------------------------------
# Choose the name of the fund.
#--------------------------------------------------------------------------------

# Change this to select your fund.
fund_name <- 'BCPBAB'
# fund_name <- 'BGVDEL'
# fund_name <- 'BGVFED'


#--------------------------------------------------------------------------------
# Specify the selected factors.
#--------------------------------------------------------------------------------


# Change this to select your explanatory variables.
# Remove the # and control+enter the selected line to use a particular approach 
# to setting the list of variables.

# Use all factors.
variable_list <- factor_list

# Specify a subset of factors by number.
# variable_list <- factor_list[5:9]

# Specify a subset of factors by name.
# variable_list <- c('S.P.500', 'FFSMB', 'FFHML')


################################################################################
# Estimation of factor models.
################################################################################

#--------------------------------------------------------------------------------
# Set up and estimate the model.
#--------------------------------------------------------------------------------

# Specify the equation to estimate.
fmla_string <- sprintf('%s ~ %s', fund_name, 
                       paste(variable_list, collapse=' + '))
fmla_string

# Convert this to a for,ula object.
fmla <- as.formula(fmla_string)
fmla

# Estimate a linear model (linear regression).
factor_model_1 <- lm(formula = fmla, data = my_data)


#--------------------------------------------------------------------------------
# Analyze and store the results.
#--------------------------------------------------------------------------------

# Print the estimation results to screen.
summary(factor_model_1)

# Save the estimates to a .csv file.
estimates <- coef(summary(factor_model_1))

output_file_name <- 'factor_model_1.csv'
write.csv(estimates, file = output_file_name)





################################################################################
# Repeat the set-up and estimation for two other funds.
################################################################################

# Can copy and paste the above or change the setings and re-run.
# Don't forget to change the fund_name, the variable_list and the output_file_name.



################################################################################
# End.
################################################################################

