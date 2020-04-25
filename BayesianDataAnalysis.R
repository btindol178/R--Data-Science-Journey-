# Bayes Data Analysis !!!!!

# The generative covid drug model

# set parameters
prop_success <- 0.42
n_infected <- 100

# Simulating data
data <- c()
for(covid in 1:n_infected) {
  data[covid] <- runif(1, min = 0, max = 1) < prop_success
}
data <- as.numeric(data)
data

# Count cured
data <- sum( as.numeric(data) )
data

# generative model using rbinom function
# this creates a binomial distribution
# Try out rbinom
rbinom(n = 1, size = 100, prob = 0.42)
# Chenge the parameters
rbinom(n = 200, size = 100, prob = 0.47)


# HOW MANY WEBSITE VISITORS WILL WE HAVE
# Fill in the parameters
n_samples <- 100000
n_ads_shown <- 100
proportion_clicks <- 0.1
n_visitors <- rbinom(n_samples, size = n_ads_shown, 
                     prob = proportion_clicks)
# Visualize n_visitors
hist(n_visitors)

# MAKING OUR PRIOR A UNIFORM DISTRIBUTION OR A CONFINDENCE INTERVAL ESSENTIALLY USING RUNIF
n_samples <- 100000
n_ads_shown <- 100

# Update proportion_clicks
proportion_clicks <- runif(n_samples, min = 0.0, max = 0.2)

n_visitors <- rbinom(n = n_samples, size = n_ads_shown, prob = proportion_clicks)

# Visualize proportion clicks
hist(proportion_clicks)

# Visualize n_visitors
hist(n_visitors)

# BAYSIAN INFERENCE MODELS (GENERATIVE MODEL AND PRIORS AND DATA)
# CONDITIONING
n_samples <- 100000
n_ads_shown <- 100

# Update proportion_clicks
proportion_clicks <- runif(n_samples, min = 0.0, max = 0.2)

n_visitors <- rbinom(n = n_samples, size = n_ads_shown, prob = proportion_clicks)

# PRIOR DATAFRAME
prior <- data.frame(proportion_clicks,n_visitors)

# now create posterior dataframe
# Create the posterior data frame
posterior <- prior[prior$n_visitors == 13, ]

# Visualize posterior proportion clicks
hist(posterior$proportion_clicks)

# HOW MANY VISITORS COULD THE SITE GET?
# Assign posterior to a new variable called prior
prior <- posterior
head(prior)

# Replace prior$n_visitors with a new sample and visualize the result
n_samples <-  nrow(prior)
n_ads_shown <- 100
prior$n_visitors <- rbinom(n_samples, size = n_ads_shown, prob = prior$proportion_clicks)
hist(prior$n_visitors)

# Calculate the probability that you will get 5 or more visitors
sum(prior$n_visitors >= 5) / length(prior$n_visitors)

# REAL LIFE APPLICATION
# Explore using the rbeta function
beta_sample <- rbeta(n = 1000000, shape1 = 1, shape2 = 1)

# Visualize the results
hist(beta_sample)

# Explore using the rbeta function
beta_sample <- rbeta(n = 1000000, shape1 = -1, shape2 = 1)

# Explore the results
head(beta_sample)

# Explore using the rbeta function
beta_sample <- rbeta(n = 1000000, shape1 = 100, shape2 = 100)

# Visualize the results
hist(beta_sample)

# Explore using the rbeta function
# So the larger the shape1 parameter is the closer the resulting distribution is to 1.0 and the larger the shape2 the closer it is to 0.
beta_sample <- rbeta(n = 1000000, shape1 = 100, shape2 = 20)

# Visualize the results
hist(beta_sample)

# IF Most ads get clicked on 5% of the time, but for some ads it is as low as 2% and for others as high as 8%.
# WE COULD ADJUST OUR PRIOR TO SUCH
beta_sample <- rbeta(n = 1000000, shape1 = 5, shape2 = 95)

# Visualize the results
hist(beta_sample)


# lets change our model according to marketings informatin
n_draws <- 100000
n_ads_shown <- 100

# Change the prior on proportion_clicks
proportion_clicks <-  rbeta(n_draws, shape1 = 5, shape2 = 95)
n_visitors <- rbinom(n_draws, size = n_ads_shown, prob = proportion_clicks)
prior <-  data.frame(proportion_clicks, n_visitors)
posterior <-  prior[prior$n_visitors == 13, ]

# This plots the prior and the posterior in the same plot
par(mfcol = c(2, 1))
hist(prior$proportion_clicks, xlim = c(0, 0.25))
hist(posterior$proportion_clicks,  xlim = c(0, 0.25))

# Let's fit the binomial model to both the video ad data 
#(13 out of 100 clicked) and the new text ad data (6 out of a 100 clicked).
# Define parameters
n_draws <- 100000
n_ads_shown <- 100
proportion_clicks <- runif(n_draws, min = 0.0, max = 0.2)
n_visitors <- rbinom(n = n_draws, size = n_ads_shown, 
                     prob = proportion_clicks)
prior <- data.frame(proportion_clicks, n_visitors)

# Create the posteriors for video and text ads
posterior_video <- prior[prior$n_visitors == 13, ] # 13 out of 100 from video ads clicked
posterior_text <- prior[prior$n_visitors == 6, ] # 6 out of 100 for text ads clicked

# Visualize the posteriors
hist(posterior_video$proportion_clicks, xlim = c(0, 0.25))
hist(posterior_text$proportion_clicks, xlim = c(0, 0.25))

# CALCULATING POSTERIOR DIFFERENCE BETWEEN TWO MODELS!!!!
posterior <- data.frame(video_prop = posterior_video$proportion_clicks[1:4000],
                        text_prop = posterior_text$proportion_click[1:4000])

# Calculate the posterior difference: video_prop - text_prop
posterior$prop_diff <- posterior$video_prop - posterior$text_prop 

# Visualize prop_diff
hist(posterior$prop_diff)


# Calculate the median of prop_diff
median(posterior$prop_diff)

# Calculate the proportion
mean(posterior$prop_diff > 0.0)

# DECISION ANALYSIS !!!!!!!!!!!!!!!!!!!!!!!!
# MORE PEOPLE WATCH VIDEO ADS BUT ARE MORE EXPENSIVE
# LESS PEOPLE LOOK AT TEXT BUT MUCH LESS EXPENSIVE!!!!
#Each visitor spends $2.53 on average, a video ad costs $0.25 and a text ad costs $0.05.
#Let's figure out the probable profit when using video ads and text ads!
visitor_spend <- 2.53
video_cost <- 0.25
text_cost <- 0.05

# Add the column posterior$video_profit
posterior$video_profit <- posterior$video_prop * visitor_spend - video_cost

# Add the column posterior$text_profit
posterior$text_profit <- posterior$text_prop * visitor_spend - text_cost

head(posterior)

# Visualize the video_profit and text_profit columns
hist(posterior$video_profit)
hist(posterior$text_profit)

# Add the column posterior$profit_diff
posterior$profit_diff <- posterior$video_profit - posterior$text_profit
head(posterior)

# Visualize posterior$profit_diff
hist(posterior$profit_diff)

# Calculate a "best guess" for the difference in profits
median(posterior$profit_diff)

# Calculate the probability that text ads are better than video ads
mean(posterior$profit_diff < 0)
#Right! Even though text ads get a lower proportion of clicks, they are also much cheaper. And, as you have calculated, 
#there is a 63% probability that text ads are better.

# put add on website and get 19 clicks per day!
# we want to model how many clicks we get per day but yes or no so we need to 
# do clicks per minute or second 
# this is possion distribution
# Simulate from a Poisson distribution and visualize the result
x <- rpois(n = 10000, lambda = 3)
hist(x)

#Let's say that you run an ice cream stand and on cloudy days you on average sell 11.5 ice creams. It's a cloudy day.
# Simulate from a Poisson distribution and visualize the result
x <- rpois(n = 10000, lambda = 11.5)
hist(x)

# Calculate the probability of break-even
mean(x >= 15)

#When you put up a banner on your friend's site you got 19 clicks in a day, how many daily clicks should you expect this banner to generate on average? Now, modify your model,
#one piece at a time, to calculate this
# Replace proportion_clicks with mean_clicks and change the parameters
n_draws <- 100000
n_ads_shown <- 100
mean_clicks <- runif(n_draws, min = 0, max = 80) # random distr from 0 - 80 clicks per day
n_visitors <- rbinom(n_draws, size = n_ads_shown, 
                     prob = proportion_clicks)

prior <- data.frame(proportion_clicks, n_visitors)
posterior <- prior[prior$n_visitors == 13, ]

# Change this model so that it uses a Poisson distribution
n_draws <- 100000
mean_clicks <- runif(n_draws, min = 0, max = 80)
n_visitors <- rpois(n = n_draws, mean_clicks)

prior <- data.frame(mean_clicks, n_visitors)
posterior <- prior[prior$n_visitors == 13, ]

hist(prior$mean_clicks)
hist(posterior$mean_clicks)

############################################################3
# Calculate the probability of drawing any of the four aces
prob_to_draw_ace <- 1/52 + 1/52 + 1/52 + 1/52
prob_to_draw_ace

# Calculate the probability of picking four aces in a row
prob_to_draw_four_aces <- 4/52 * 3/51 * 2/50 * 1/49
prob_to_draw_four_aces

#imulates the number of clicks/visitors (n_clicks) from 100 shown ads using the rbinom function given
#that the underlying proportion of clicks is 10%.
# Rewrite this code so that it uses dbinom instead of rbinom
# dbinom returns a probability

#calculates the probability of getting 13 visitors (prob_13_visitors).
#That is, in probability notation it's calculating P(n_visitors = 13 | proportion_clicks = 10%).
n_ads_shown <- 100
proportion_clicks <- 0.1
prob_13_visitors <- dbinom(13, size = n_ads_shown, prob = proportion_clicks)
prob_13_visitors

# Explore using dbinom to calculate probability distributions
n_ads_shown <- 100
proportion_clicks <- 0.1
n_visitors <- seq(0, 100, by = 1)
prob <- dbinom(n_visitors, 
               size = n_ads_shown, prob = proportion_clicks)
prob

# Plot the distribution
plot(n_visitors, prob, type = "h")

# CALCULATING JOINT DISTRIBUTION
n_ads_shown <- 100
proportion_clicks <- seq(0, 1, by = 0.01)
n_visitors <- seq(0, 100, by = 1)
pars <- expand.grid(proportion_clicks = proportion_clicks,
                    n_visitors = n_visitors)
pars$prior <- dunif(pars$proportion_clicks, min = 0, max = 0.2)
pars$likelihood <- dbinom(pars$n_visitors,size = n_ads_shown, prob = pars$proportion_clicks)

# Add the column pars$probability and normalize it
pars$probability <- pars$likelihood * pars$prior
pars$probability <- pars$probability / sum(pars$probability)

# Let's resurrect the zombie site example where you tested text ads. Out of a 100 impressions of the text ad, 6 out of a 100 clicked and visited your site.

#To the right is roughly the code you developed in the last exercise. pars is currently the joint distribution over all
# combinations of proportion_clicks and n_visitors.
# Condition on the data 
pars <- pars[pars$n_visitors == 6, ]
# Normalize again
pars$probability <- pars$probability / sum(pars$probability)
# Plot the posterior pars$probability
plot(pars$proportion_clicks, pars$probability, type = "h")

# BAYES THEORM!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Assign mu and sigma
mu <- 3500
sigma <- 600

weight_distr <- rnorm(n = 100000, mean = mu, sd = sigma)
hist(weight_distr, 60, xlim = c(0, 6000), col = "lightgreen")

# Create weight
weight <- seq(0, 6000, by = 100)

# Calculate likelihood
likelihood <- dnorm(weight, mu, sigma)

# Plot the distribution of weight
plot(weight, likelihood, type = "h")

########################################################
# The IQ of a bunch of zombies
iq <- c(55, 44, 34, 18, 51, 40, 40, 49, 48, 46)
# Defining the parameter grid
pars <- expand.grid(mu = seq(0, 150, length.out = 100), 
                    sigma = seq(0.1, 50, length.out = 100))
# Defining and calculating the prior density for each parameter combination
pars$mu_prior <- dnorm(pars$mu, mean = 100, sd = 100)
pars$sigma_prior <- dunif(pars$sigma, min = 0.1, max = 50)
pars$prior <- pars$mu_prior * pars$sigma_prior
# Calculating the likelihood for each parameter combination
for(i in 1:nrow(pars)) {
  likelihoods <- dnorm(iq, pars$mu[i], pars$sigma[i])
  pars$likelihood[i] <- prod(likelihoods)
}
# Calculate the probability of each parameter combination
pars$probability <- pars$likelihood * pars$prior / sum(pars$likelihood * pars$prior)

# CALCULATING IF SHOULD HAVE POOL PARTY
head(pars)
sample_indices <- sample( nrow(pars), size = 10000,
                          replace = TRUE, prob = pars$probability)
head(sample_indices)

# Sample from pars to calculate some new measures
pars_sample <- pars[sample_indices, c("mu", "sigma")]

# Visualize the mean IQ
hist(pars_sample$mu)

# Calculate quantiles
quantile(pars_sample$mu, c(0.025, 0.5, 0.975))

head(pars_sample)
pred_iq <- rnorm(10000, mean = pars_sample$mu, 
                 sd = pars_sample$sigma)

# Visualize pred_iq
hist(pred_iq)

head(pars_sample)
pred_iq <- rnorm(10000, mean = pars_sample$mu, 
                 sd = pars_sample$sigma)

# Visualize pred_iq
hist(pred_iq)

# Calculate the probability of a zombie being "smart" (+60 IQ)
sum(pred_iq >= 60) / length(pred_iq)

# MONTECARLO WITH BASION
#The t-test is a classical statistical procedure used to compare the means of two data sets. In 2013 John Kruschke developed a souped-up Bayesian version of the t-test he named BEST 
#(standing for Bayesian Estimation Supersedes the t-test). 
# The IQ of zombies on a regular diet and a brain based diet.
iq_brains <- c(44, 52, 42, 66, 53, 42, 55, 57, 56, 51)
iq_regular <- c(55, 44, 34, 18, 51, 40, 40, 49, 48, 46)

# Calculate the mean difference in IQ between the two groups
mean(iq_brains) - mean(iq_regular)

# Fit the BEST model to the data from both groups
install.packages("BEST")
library(BEST)
best_posterior <- BESTmcmc(iq_brains, iq_regular)


# Plot the model result
plot(best_posterior)

# The IQ of zombies given a regular diet and a brain based diet.
iq_brains <- c(44, 52, 42, 66, 53, 42, 55, 57, 56, 51)
iq_regular <- c(55, 44, 34, 18, 51, 40, 40, 49, 48, 150) # <- Mutant zombie

# Modify the data above and calculate the difference in means
mean(iq_brains) - mean(iq_regular)

# Fit the BEST model to the modified data and plot the result
library(BEST)
best_posterior <- BESTmcmc(iq_brains, iq_regular)
plot(best_posterior)
