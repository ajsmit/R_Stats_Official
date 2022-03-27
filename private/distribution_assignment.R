# -------------------------------------------------------------------------
#
# _private/distribution_assignment.R
# Author: Prof. AJ Smit
# Date: 28 February 2020
#
# BASIC STATISTICS
#
# -------------------------------------------------------------------------


# Load some packages ------------------------------------------------------

library(fitdistrplus)
library(logspline)


# Functions to generate data of a specified distribution ------------------

# DISCRETE DISTRIBUTIONS

# Bernoulli distribution
# rbern()

# Binomial distribution
# rbinom()

# Negative binomial distribution
# rnbinom()

# Geometric distribution
# rgeom()

# Poisson distribution
# rpois()

# CONTINUOUS DISTRIBUTIONS

# Normal distribution
# rnorm()

# Uniform distribution
# runif()

# Log-normal distribution
# rlnorm()

# Student T distribution
# rt()

# Chi-squared distribution
# rchisq()

# Exponential distribution
# rexp()

# F distribution
# rf()

# Gamma distribution
# rgamma()

# Beta distribution
# rbeta()


# FOR EXAMPLE -------------------------------------------------------------
# Generate log-normal data ------------------------------------------------

y <- rlnorm(100, 13, 3)
par(mfrow = c(2, 2))
plot(x = c(1:length(y)), y = y)
hist(y)
descdist(y, discrete = FALSE, boot = 100)


# Normally distributed data -----------------------------------------------

y <- rnorm(100, 13, 2)
par(mfrow = c(2, 2))
plot(x = c(1:100), y = y)
hist(y)
descdist(y, discrete = FALSE)


# Uniformly distributed data (1) ------------------------------------------

y <- runif(100)
par(mfrow = c(2, 2))
plot(x = c(1:100), y = y)
hist(y)
descdist(y, discrete = FALSE)


# Uniformly distributed data (2) ------------------------------------------

y <- rexp(100, 0.7)
par(mfrow = c(2, 2))
plot(x = c(1:100), y = y)
hist(y)
descdist(y, discrete = FALSE)


# Other approaches to testing for normal distributions --------------------

y <-rnorm(n = 200, m = 13, sd = 2)
par(mfrow = c(2, 2))
# using some basic base graphics as ggplot2 is overkill;
# we can get a histogram using hist() statement
hist(y, main = "Histogram of observed data")
plot(density(y), main = "Density estimate of data")
plot(ecdf(y), main = "Empirical cumulative distribution function")
# standardise the data
z.norm <- (y - mean(y)) / sd(y)
# make a qqplot
qqnorm(z.norm)
# add a 45-degree reference line
abline(0, 1)


# ASSIGNMENT --------------------------------------------------------------

# Task 1
# Of all the distributions above, select the ones that have direct applicability
# to the the kinds of data that biologists and/or environmental scientists could
# reasonably encounter during their careers.
#
# For each, justify your answer, and provide an example of a survey, study, or
# experiment that has resulted on data of each of the specified kinds of distributions.
#
# (Hint: there is 1 discrete distribution and 3 or 4 continuous distributions that
# fit the bill)

# Task 2
# Using the functions listed above, simulate data for each of the data distribution
# types that you identified in Task 1 in order to create a mock dataset for a fictitious
# experiment. For each, create contrasting conditions if/when possible and plot the data
# in ggplot2 in order to demonstrate the outcome of your 'experiment'.
#
# For each, write a moderately convincing Methods section (0.5 page max for each)
# that describes the experiment, and write the accompanying Results section (also <0.5
# page each). The Results must include the various graphical displays (incl. the Normal
# Q-Q Plots, Histograms, and anything else that is appropriate for the data in question).
#
# Discuss concisely why the data distribution type is appropriate for the experiments.
