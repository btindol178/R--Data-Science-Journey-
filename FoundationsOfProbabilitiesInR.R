# Foundations of probability in R
##################################################################################################
##################################################################################################
##################################################################################################
# Making random T or F (yes or no) outcomes with rbinom
library(ggplot2)
#This function we want to see 1 observation with 1 trail per observation and 50% chance of sucess
rbinom(1,1,.50) # 1 is default

# This rolls 2 times with 1 trial per observation and 50% chance
twice<- rbinom(2,1,.50);twice

# This rolls 2 times with 1 trial per observation and 75% chance
z <- rbinom(2,1,.75);z

# here we roll 10 times with 10 trials per observation and 70%chance
rbinom(10,10,.7)

#############################################
# Cumulative density

# We want to see how likely we would get out of 10 flips 5 yes and 5 no with 50% prob
flips <- rbinom(10000,10,.5);flips
hist(flips) # we can see 5 is most likely 25000 is 

# how many was equal to 5
table(flips ==5) # 24000
mean(flips == 5)#24.6 % chance

# Calculating the exact probability density
# 5 the outcome were estimating density at 10 is the number of coins and probability .5
dbinom(5,10,.5)

# proability of getting 4 
dbinom(4,10,.5) # 

#probability of getting 10
dbinom(10,10,.5) # 

# what percent is less than or equal to 4 or fewer 
flips <- rbinom(100000, 10,.50)
mean(flips <= 4) # 37% of simulations

# Get exact cumulative density 
pbinom(4,10,.5)

# Calculate the probability that 2 are heads using dbinom
z <-dbinom(2, 10, .3);z

# Confirm your answer with a simulation using rbinom
mean(rbinom(10000, 10, .3) == 2)

# Calculate the probability that at least five coins are heads
1 - pbinom(4, 10, .3)

# Confirm your answer with a simulation of 10,000 trials
mean(rbinom(10000, 10, .3) >= 5)

# Here is how you computed the answer in the last problem
mean(rbinom(10000, 10, .3) >= 5)

# Try now with 100, 1000, 10,000, and 100,000 trials
mean(rbinom(100, 10, .3) >= 5)
mean(rbinom(1000, 10, .3) >= 5)
mean(rbinom(10000, 10, .3) >= 5)
mean(rbinom(100000, 10, .3) >= 5)
mean(rbinom(1000000, 10, .3) >= 5)

########################################################################################################
# Expected value and variance
# expected value is mean value
flips <- rbinom(100000,10,.5)

# expected value
mean(flips)

# sampling from 100 and prob .2
mean(rbinom(10000,100,.2))

# Variance
X <- rbinom(10000,10,.5)
var(X) # 2.5 is the average squared distance between 5 and one random draw

#What is the expected value of a binomial distribution where 25 coins are flipped, each having a 30% chance of heads?
# Calculate the expected value using the exact formula
25 * .3

# Confirm with a simulation using rbinom
mean(rbinom(10000, 25, .3))

#What is the variance of a binomial distribution where 25 coins are flipped, each having a 30% chance of heads?
# Calculate the variance using the exact formula
25 * .3 * (1 - .3)

# Confirm with a simulation using rbinom
var(rbinom(10000, 25, .3))

###################################################################################################################
# Probability of event A and event B
A <- rbinom(100000,1,.5);A
B <- rbinom(100000,1,.5);B

A & B

# A and B were true 25% of time
# same thing as .5 * .5 = .25
mean(A & B)

# Again .1 * .7 = .07%
A <- rbinom(100000,1,.1)
B <- rbinom(100000,1,.7)
mean(A & B)

# If events A and B are independent, and A has a 40% chance of happening, and event B has a 20% chance of happening, what is the probability they will both happen?
.4*.2 # .08%


# Simulate 100,000 flips of a coin with a 40% chance of heads
A <- rbinom(100000, 1, .4)

# Simulate 100,000 flips of a coin with a 20% chance of heads
B <- rbinom(100000, 1, .2)

# Estimate the probability both A and B are heads
mean(A & B)

# You've already simulated 100,000 flips of coins A and B
A <- rbinom(100000, 1, .4)
B <- rbinom(100000, 1, .2)

# Simulate 100,000 flips of coin C (70% chance of heads)
C <- rbinom(100000, 1, .7)

# Estimate the probability A, B, and C are all heads
mean(A & B & C)

#####################################################################################
#Probability of A OR B
#Prob of A
A <- rbinom(100000, 1, .5)
# Probability of B
B <- rbinom(100000, 1, .5)

# A or B
# P(A or B) = Pr(A) + Pr(B)-Pr(A and B)
# .5 + .5 - .5 * .5 = .75
mean(A|B)

# Lets 3 events A or B or C
A <- rbinom(100000, 1, .5)
# Probability of B
B <- rbinom(100000, 1, .5)
# Prob of c
C <- rbinom(100000, 1, .5)

mean(A|B|C)

# If coins A and B are independent, and A has a 60% chance of coming up heads, and event B has a 10% chance of coming up heads, what is the probability either A or B will come up heads?
A <- rbinom(100000, 1, .6)
# Probability of B
B <- rbinom(100000, 1, .1)
mean(A|B)
.6 + .1 - (.6*.1)


# Simulate 100,000 flips of a coin with a 60% chance of heads
A <- rbinom(100000, 1, .6)

# Simulate 100,000 flips of a coin with a 10% chance of heads
B <- rbinom(100000, 1, .1)

# Estimate the probability either A or B is heads
mean(A | B)

# Use rbinom to simulate 100,000 draws from each of X and Y
X <- rbinom(100000, 10, .6)
Y <- rbinom(100000, 10, .7)

# Estimate the probability either X or Y is <= to 4
mean(X <= 4 | Y <= 4)

# Use pbinom to calculate the probabilities separately
prob_X_less <- pbinom(4, 10, .6);prob_X_less
prob_Y_less <- pbinom(4, 10, .7);prob_Y_less

#####################################################################
# Multiplying random variables
X <- rbinom(100000,10,.5)
mean(X)
Y <- 3*X
mean(Y)

# Now get variance
var(X)
var(Y)

#If X is a binomial with size 50 and p = .4, what is the expected value of 3*X?
X <- rbinom(100000,50,.4)
Y <- X*3
mean(Y)


# Simulate 100,000 draws of a binomial with size 20 and p = .1
X <- rbinom(100000, 20, .1)

# Estimate the expected value of X
mean(X)

# Estimate the expected value of 5 * X
mean(5 * X)

# X is simulated from 100,000 draws of a binomial with size 20 and p = .1
X <- rbinom(100000, 20, .1)

# Estimate the variance of X
var(X)

# Estimate the variance of 5 * X
var(5 * X)

# Adding two random variables
# The expected value of x + y is the expected value of x plus the expected value of y
X <- rbinom(100000, 10, .5)
mean(X) # 5 
# make Y
Y <- rbinom(100000, 100, .2)
mean(Y) # 20
# Combine
Z <- X+Y 
mean(Z)# 25

# Now look at variance
# Same rule for expected value with variance
var(X)
var(Y)
var(Z)

# Simulate 100,000 draws of X (size 20, p = .3) and Y (size 40, p = .1)
X <- rbinom(100000, 20, .3) 
Y <- rbinom(100000, 40, .1)

# Estimate the expected value of X + Y
mean(X + Y)

# Simulation from last exercise of 100,000 draws from X and Y
X <- rbinom(100000, 20, .3) 
Y <- rbinom(100000, 40, .1)

# Find the variance of X + Y
var(X + Y)

# Find the variance of 3 * X + Y
var(3 * X + Y)

##################################################################################################
# Updating from evidence

# two piles of 50,000 coins
fair <- rbinom(50000,20,.5)
fairsum <- sum(fair == 14)

biased <- rbinom(50000,20,.75)
biasedsum <- sum(biased == 14)

# total that resulted in 14
fairsum + biasedsum

# get the percentage
# Pr(biased|14 heads)
# #Biased 2/14 heads/ total 2/14 Heads
biasedsum/(biasedsum+fairsum) # 81% chance the coin is biased 


# Simulate 50000 cases of flipping 20 coins from fair and from biased
fair <- rbinom(50000, 20, .5)
biased <- rbinom(50000, 20, .75)

# How many fair cases, and how many biased, led to exactly 11 heads?
fair_11 <- sum(fair == 11)
biased_11 <- sum(biased == 11)

# Find the fraction of fair coins that are 11 out of all coins that were 11
fair_11 / (fair_11 + biased_11)


# Simulate 50000 cases of flipping 20 coins from fair and from biased
fair <- rbinom(50000, 20, .5)
biased <- rbinom(50000, 20, .75)

# How many fair cases, and how many biased, led to exactly 16 heads?
fair_16 <- sum(fair == 16)
biased_16 <- sum(biased == 16)

# Find the fraction of fair coins that are 16 out of all coins that were 16
fair_16 / (fair_16 + biased_16)

##########################################################################################
#Prior probability

fair <- rbinom(90000,20,.5)
fairtotal <- sum(fair == 14)

baised <- rbinom(10000,20,.75)
baisedtotal <- sum(baised == 14)

# num of baised w/14 heads/# total w/14 heads
baisedtotal/(baisedtotal + fairtotal)

# Simulate 8000 cases of flipping a fair coin, and 2000 of a biased coin
fair_flips <- rbinom(8000, 20, .5)
biased_flips <- rbinom(2000, 20, .75)

# Find the number of cases from each coin that resulted in 14/20
fair_14 <- sum(fair_flips == 14)
biased_14 <- sum(biased_flips == 14)

# Use these to estimate the posterior probability
fair_14 / (fair_14 + biased_14)

# Simulate 80,000 draws from fair coin, 10,000 from each of high and low coins
flips_fair <- rbinom(80000, 20, .5)
flips_high <- rbinom(10000, 20, .75)
flips_low <- rbinom(10000, 20, .25)

# Compute the number of coins that resulted in 14 heads from each of these piles
fair_14 <- sum(flips_fair == 14)
high_14 <- sum(flips_high == 14)
low_14 <- sum(flips_low == 14)

# Compute the posterior probability that the coin was fair
fair_14 / (fair_14 + low_14 + high_14)

###############################################################################################
# Bayes Theorem
fair <- rbinom(90000,20,.5)
sum(fair == 14)
# finding exact probability of 14 heads multiplied by prior that the coin was fair .9 (this is the probability the coin is fair and has 14 heads)
prob_14_fair <-dbinom(14,20,.5)*.9# time

baised <- rbinom(10000,20,.75)
sum(baised == 14)
prob_14_baised <-dbinom(14,20,.75) *.1

# Bayes therom 
# A was if it was biased and B was 14 heads
# we had info on if the coin was 14 heads given that it was baised but we needed is the coin biased given that the coin has 14 heads
prob_14_baised/ (prob_14_fair + prob_14_baised )


# Use dbinom to calculate the probability of 11/20 heads with fair or biased coin
probability_fair <- dbinom(11, 20, .5)
probability_biased <- dbinom(11, 20, .75)

# Calculate the posterior probability that the coin is fair
probability_fair / (probability_fair + probability_biased)

# Find the probability that a coin resulting in 14/20 is fair
dbinom(14, 20, .5) / (dbinom(14, 20, .5) + dbinom(14, 20, .75))

# Find the probability that a coin resulting in 18/20 is fair
dbinom(18, 20, .5) / (dbinom(18, 20, .5) + dbinom(18, 20, .75))

# Use dbinom to find the probability of 16/20 from a fair or biased coin
probability_16_fair <- dbinom(16, 20, .5)
probability_16_biased <- dbinom(16, 20, .75)

# Use Bayes' theorem to find the posterior probability that the coin is fair
(probability_16_fair * .99) / (probability_16_fair * .99 + probability_16_biased * .01)

####################################################################################################################################
# The normal Distribution
# 100000 draws from normal dist with size 1000 and probability .5
binomial <- rbinom(100000,1000,.5);hist(binomial) 
expected_value <- 1000*.5;expected_value
variance <- 1000 *.5 *(1-.5) ;variance
stdev <- sqrt(variance);stdev

# simulate normal with these parameters
normal <-rnorm(100000,expected_value,stdev);hist(normal);

# compare binomial vs normal dist
hist(binomial);hist(normal)

# Draw a random sample of 100,000 from the Binomial(1000, .2) distribution
binom_sample <- rbinom(100000, 1000, .2);hist(binom_sample)

# Draw a random sample of 100,000 from the normal approximation
normal_sample <- rnorm(100000, 200, sqrt(160));hist(normal_sample)

# Simulations from the normal and binomial distributions
binom_sample <- rbinom(100000, 1000, .2);hist(binom_sample)
normal_sample <- rnorm(100000, 200, sqrt(160));hist(normal_sample)

# Use binom_sample to estimate the probability of <= 190 heads
mean(binom_sample <= 190)

# Use normal_sample to estimate the probability of <= 190 heads
mean(normal_sample <= 190)

# Calculate the probability of <= 190 heads with pbinom
pbinom(190, 1000, .2)

# Calculate the probability of <= 190 heads with pnorm
pnorm(190, 200, sqrt(160))

# Draw a random sample of 100,000 from the Binomial(10, .2) distribution
binom_sample <- rbinom(100000, 10, .2)

# Draw a random sample of 100,000 from the normal approximation
normal_sample <- rnorm(100000, 2, sqrt(1.6))

##########################################################################################################
#Poisson Distribution
# How often a rare event happens out of a large sample

# example of poisson  distribution
binomial <- rbinom(100000,1000,1/1000);hist(binomial)

# Poisson distribution
# first parameter is number of trial and the 1 is mean 
# parameters mean and value dont need that it is out of 1000 coins
poisson <- rpois(100000,1);hist(poisson)

# variance is equal to the mean in poisson it can have any mean as long as its positive
# when modeling rare events as counts
# modling a book store and how many people enter an hour

# Draw a random sample of 100,000 from the Binomial(1000, .002) distribution
binom_sample <- rbinom(100000, 1000, .002);hist(binom_sample)

# Draw a random sample of 100,000 from the Poisson approximation
# 
poisson_sample <- rpois(100000, 2);hist(poisson_sample)

# Simulate 100,000 draws from Poisson(2)
poisson_sample <- rpois(100000, 2);hist(poisson_sample)

# Find the percentage of simulated values that are 0
mean(poisson_sample == 0)

# Use dpois to find the exact probability that a draw is 0
dpois(0, 2)

# Simulate 100,000 draws from Poisson(1)
X <- rpois(100000, 1);hist(X)

# Simulate 100,000 draws from Poisson(2)
Y <- rpois(100000, 2);hist(Y)

# Add X and Y together to create Z
Z <- X + Y;hist(Z)

# Simulate 100,000 draws from Poisson(2)
W <- rpois(100000, 3);hist(W)

#######################################################################################
# Geometric distribution
# keep flipping until i get heads how many times until you get that even 

# using rbinom
flips <- rbinom(100,1,.1);flips
which(flips ==1) # which flips were head
which(flips ==1)[1] # the 6th flip was first head

# all in one
which(rbinom(100,1,.1) == 1)[1]

# not having to repeat the above code 10 times
replicate(10,which(rbinom(100,1,.1) == 1)[1])

# use rgeom for geometric dist
# first parameter is the number of draws and seconds is the probability of the event your looking for
geom <- rgeom(100000,.1);hist(geom)

#when each coin has a 10% chance of heads on average it will be the 9th coin
mean(geom)

# E(X) = 1/probability -1
# Simulate 100 instances of flipping a 20% coin
flips <- rbinom(100, 1, .2)

# Use which to find the first case of 1 ("heads")
which(flips == 1)[1]

# Existing code for finding the first instance of heads
which(rbinom(100, 1, 0.2) == 1)[1]

# Replicate this 100,000 times using replicate()
replications <- replicate(100000, which(rbinom(100, 1, 0.2) == 1)[1])

# Histogram the replications with qplot
qplot(replications)

# Replications from the last exercise
replications <- replicate(100000, which(rbinom(100, 1, .2) == 1)[1]);hist(replications)

# Generate 100,000 draws from the corresponding geometric distribution
geom_sample <- rgeom(100000, .2);hist(geom_sample)

# Find the probability the machine breaks on 5th day or earlier
pgeom(4, .1)

# Find the probability the machine is still working on 20th day
1 - pgeom(19, .1)

# Calculate the probability of machine working on day 1-30
still_working <- 1 - pgeom(0:29, .1);hist(still_working)

# Plot the probability for days 1 to 30
qplot(1:30, still_working)
