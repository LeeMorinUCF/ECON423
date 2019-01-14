################################################################################
# 
# ECON 423 Sample Program
# Introduction to Portfolio Theory
# Lee Morin, PhD
# Department of Economics, Queen's University
# Original version: Jan. 13, 2012
# Last modified: Jan. 22, 2017
# 
# ECON423PortfolioIntro.R gives some examples of the calculation of the 
# portfolio frontier for a number of introductory cases with two assets.
# Adapted from a program written by Pierre Chausse. 
# 
# Input: None.
# Output: Prints data and plots figures.
# Dependencies: None (calls a number of user-defined functions within the program).
# 
################################################################################



################################################################################
# One risky asset and one risk-free asset
################################################################################

sig <- seq(0,.3, length.out=100)
plot(sig, 0.02 + 0.08*(sig/0.05), 
     type="l", 
     xlab="Sigma", 
     ylab="E(r)", 
     main="Portfolio frontier: 1 risky and 1 risk-less")
abline(v=0, lwd=2)
text(0.05, 0.3, "w=1")
arrows(0.05, 0.29, 0.05, 0.1)



################################################################################
# Two risky assets
################################################################################

#--------------------------------------------------------------------------------
# Need the following functions first:
#--------------------------------------------------------------------------------

# function getInfo calculates parameters for the portfolio frontier 
#   from the mean and covariance of returns on two assets.

getInfo <- function(mu1, mu2, sig1, sig2, rho)
{
	wm <- (sig2 - sqrt(sig1*sig2)*rho) / (sig1 + sig2 - 2*sqrt(sig1*sig2)*rho)
	ewm <- mu2 + (mu1 - mu2)*wm
	sigm <- wm^2 * sig1 + (1 - wm)^2 * sig2 + 2*wm*(1 - wm)*sqrt(sig1*sig2)*rho
	mup <- seq( - 2*ewm, 4*ewm, length.out=100)
	w <- (mup - mu2) / (mu1 - mu2)
	sig <- sqrt(w^2 * sig1 + (1 - w)^2 * sig2 + 2*w*(1 - w)*sqrt(sig1*sig2)*rho)
	e <- mu2 + (mu1 - mu2)*w
	ans <- list(e=e, w=w, mup=mup, sigm=sigm, ewm=ewm, wm=wm, 
	            sig=sig, mu1=mu1, mu2=mu2, sig1=sig1, sig2=sig2, rho=rho)
}


# function getPF uses the parameters calculate from function getInfo 
#   to plot the efficient frontier from two risky assets.

getPf <- function(mu1, mu2, sig1, sig2, rho, add=F, info=T, ...)
{
	res <- getInfo(mu1, mu2, sig1, sig2, rho)
	if(!add)
	{
		plot(res$sig, res$e, type="l", 
		     xlab="sigma", ylab="E(r)",
		     main="Portfolio frontier: 2 risky assets", 
		     xlim=c(0, max(res$sig)), ...)
		if (info)
		{
  		points(sqrt(sig1), mu1, pch=21, col=2, bg=2)
  		points(sqrt(sig2), mu2, pch=21, col=3, bg=3)
  		points(sqrt(res$sigm), res$ewm, pch=21, col=4, bg=4)
  		legend("right", c("Asset 1","Asset 2", "MVP"), pch=c(21,21,21), col=2:4)
  		arrows(sqrt(res$sigm), res$ewm, sqrt(res$sigm)*(2), res$ewm)
  		text(sqrt(res$sigm)*(2), res$ewm, paste("w = ", round(res$wm, 4), sep=""), pos=4)
		}
	}
	else
		lines(res$sig, res$e, ...)
	res
}

#--------------------------------------------------------------------------------
# End functions
#--------------------------------------------------------------------------------


################################################################################
# Example 1: Uncorrelated assets.
################################################################################

res <- getPf(0.05, 0.1, 0.0025, 0.01, 0)


################################################################################
# Example 2: Negatively Correlated assets.
################################################################################

res <- getPf(0.05, 0.1, 0.0025, 0.01, -0.25)


#--------------------------------------------------------------------------------
# Create a function for adding the tangency line:
#--------------------------------------------------------------------------------

# function getTang calculates and plots the tangency line for a portfoli frontier
#   defined by the output from function getPf.

getTang <- function(res, rf)
{
	if (rf > res$ewm)
  	stop("The risk-free rate must be smaller than the expected return on the MVP.")
	V1 <- res$mu1 - rf
	V2 <- res$mu2 - rf
	num <- V1*res$sig2 - V2*res$rho*sqrt(res$sig1*res$sig2)
	den <- V1*res$sig2 + V2*res$sig1 - (V1 + V2)*res$rho*sqrt(res$sig1*res$sig2)
	w1 <- num/den
	w2 <- 1 - w1
	sig <- sqrt(w1^2 * res$sig1 + w2^2 * res$sig2 + 2*w1*w2*sqrt(res$sig1*res$sig2)*res$rho)
	e <- res$mu2 + (res$mu1 - res$mu2)*w1
	abline(rf, (e - rf)/sig, col=6, lty=2, lwd=2)
	points(sig, e, pch=21, col=6, bg=6)
	legend("topleft", c("Tang. Portfolio", "Efficient Frontier"), pch=c(21), col=c(6,6), lty=c(0,2))
}

#--------------------------------------------------------------------------------
# End function
#--------------------------------------------------------------------------------


################################################################################
# Example 2: Negatively Correlated assets (with tangency line).
################################################################################


res <- getPf(.05,.1,.0025,.01,-.25)
getTang(res,.05)




################################################################################
# End
################################################################################
