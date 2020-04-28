# Bayes Regression compared to Linear Regression
install.packages("tidyverse")
library(tidyverse)
library(dplyr)
library(broom)
songs <- read.csv("spotifydata.csv")
colnames(songs)[1] <- "track_name"

head(songs)
str(songs)

#Frequentist linear regression
# Create the model here
lm_model <- lm(popularity ~ song_age, data = songs)

# Produce the summary
summary(lm_model)

# Print a tidy summary of the coefficients
tidy(lm_model)

install.packages("rstanarm")
library(rstanarm)
install.packages("pkgbuild")
library(pkgbuild)
update.packages(checkBuilt=TRUE)

# Create the model here
stan_model <- stan_glm(popularity ~ song_age, data = songs)

# Produce the summary
summary(stan_model)# this gives a distribution instead of a point estimate

# Print a tidy summary of the coefficients
tidy(stan_model)

#Frequentist: parameters are fixed, data is random
#Bayesian: parameters are random, data is fixed
#What's a p-value?
#Probability of test statistic, given null hypothesis
#So what do Bayesians want?
#Probability of parameter values, given the observed data
# Confidence interval: Probability that a range contains the true value
###There is a 90% probability that the true value falls within this range

# Create the 90% credible intervals
posterior_interval(stan_model)

# Create the 95% credible intervals
posterior_interval(stan_model, prob = 0.95)

# Create the 80% credible intervals
posterior_interval(stan_model, prob = 0.80)

#By changing the size of the posterior, we can change the number of samples used for the posterior summaries, and impact the estimation time. The songs data is already loaded.
# 3 chains, 1000 iterations, 500 warmup
model_3chains <- stan_glm(popularity ~ song_age, data = songs,
                          chains = 3, iter = 1000, warmup = 500)

# Print a summary of model_3chains
summary(model_3chains)

# 2 chains, 100 iterations, 50 warmup
model_2chains <- stan_glm(popularity ~ song_age, data = songs,
                          chains = 2, iter = 100, warmup = 50)

# Print a summary of model_2chains
summary(model_2chains)

# Determine Prior Distributions
# Estimate the model
stan_model <- stan_glm(popularity ~ song_age, data = songs)

# Print a summary of the prior distributions
prior_summary(stan_model)

# Calculate adjusted scales
# Calculate the adjusted scale for the intercept
10 * sd(songs$popularity)

# Calculate the adjusted scale for `song_age`
(2.5 / sd(songs$song_age)) * sd(songs$popularity)

# Calculate the adjusted scale for `valence`
(2.5 / sd(songs$valence)) * sd(songs$popularity)

# UNADJUSTED PRIORS
# Estimate the model with unadjusted scales
no_scale <- stan_glm(popularity ~ song_age, data = songs,
                     prior_intercept = normal(autoscale = FALSE),
                     prior = normal(autoscale = FALSE),
                     prior_aux = exponential(autoscale = FALSE)
)

# Print the prior summary
prior_summary(no_scale)

# Changing Priors
# Estimate a model with flat priors
flat_prior <- stan_glm(popularity ~ song_age, data = songs,
                       prior_intercept = NULL, prior = NULL, prior_aux = NULL)

# Print a prior summary
prior_summary(flat_prior)

# specifying informative priors
# Estimate the model with an informative prior
inform_prior <- stan_glm(popularity ~ song_age, data = songs,
                         prior = normal(location = 20, scale = 0.1, autoscale = FALSE))

# Print the prior summary
prior_summary(inform_prior)

#Altering the Estimation Process
# Estimate the model with a new `adapt_delta`
adapt_model <- stan_glm(popularity ~ song_age, data = songs,
                        control = list(adapt_delta = 0.99))

# View summary
summary(adapt_model)

# Estimate the model with a new `max_treedepth`
tree_model <- stan_glm(popularity ~ song_age, data = songs,
                       control = list(max_treedepth = 15))

# View summary
summary(tree_model)

# Using R2 statistic frequentist
# Print the R-squared from the linear model
lm_summary$r.squared

# Calulate sums of squares
ss_res <- var(residuals(lm_model))
ss_fit <- var(fitted(lm_model))

# Calculate the R-squared
1 - (ss_res / (ss_res + ss_fit))

#Bayesian R squared
# Save the variance of residulas
ss_res <- var(residuals(stan_model))

# Save the variance of fitted values
ss_fit <- var(fitted(stan_model))

# Calculate the R-squared
1 - (ss_res / (ss_res + ss_fit))

# Predicted scores distributions
# Calculate posterior predictive scores
predictions <- posterior_linpred(stan_model)

# Print a summary of the observed data
summary(songs$popularity)

# Print a summary of the 1st replication
summary(predictions[1,])

# Print a summary of the 10th replication
summary(predictions[10,])

# Calculate the posterior distribution of the R-squared
r2_posterior <- bayes_R2(stan_model)

# Make a histogram of the distribution
hist(r2_posterior)

# Posterior Predictive Testing
# Create density comparison
pp_check(stan_model, "dens_overlay")

# Create scatter plot of means and standard deviations
pp_check(stan_model, "stat_2d")

# Bayesian Model Comparision
# using loo package
install.packages("loo")
library(loo)

# Estimate the model with 1 predictor
model_1pred <- stan_glm(popularity ~ song_age, data = songs)

# Print the LOO estimate for the 1 predictor model
loo(model_1pred)

# Estimate the model with both predictors
model_2pred <- stan_glm(popularity ~ song_age * artist_name, data = songs)

# Print the LOO estimates for the 2 predictor model
loo(model_2pred)

#Awesome! In the summary, we see that the model with two predictors has LOO approximation of -865.0, and the model with one predictor has a LOO approximation of -888.1. Because the two predictor model is higher (less negative), we would expect that model to have better predictions. However, in order to know if this increase is meaningful,
#we need to direclty compare the model and compare the difference to the standard error.

# Visualizing Bayesian Model
library(ggplot2)
# Save the model parameters
tidy_coef <- tidy(stan_model)

# Extract intercept and slope
model_intercept <- tidy_coef$estimate[1]
model_slope <- tidy_coef$estimate[2]

# Create the plot
ggplot(songs, aes(x = song_age, y = popularity)) +
  geom_point() +
  geom_abline(intercept = model_intercept, slope = model_slope)

# Save the values from each draw of the posterior distribution
draws <- spread_draws(stan_model, `(Intercept)`, `song_age`)

# Print the `draws` data frame to the console
draws

# Create the plot
ggplot(songs, aes(x = song_age, y = popularity)) +
  geom_point() +
  geom_abline(data = draws, aes(intercept = `(Intercept)`, slope = song_age),
              size = 0.1, alpha = 0.2, color = "skyblue")

# Making Predictions!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Estimate the regression model
stan_model <- stan_glm(popularity ~ song_age + artist_name, data = songs)

# Print the model summary
summary(stan_model)

# Get posteriors of predicted scores for each observation
posteriors <- posterior_predict(stan_model)

# Print 10 predicted scores for 5 songs
posteriors[1:10, 1:5]

# Create data frame of new data
predict_data <- data.frame(song_age = 663, artist_name = "Beyoncé")

# Create posterior predictions for Lemonade album
new_predictions <- posterior_predict(stan_model, newdata = predict_data)

# Print first 10 predictions for the new data
new_predictions[1:10,]

# Print a summary of the posterior distribution of predicted popularity
summary(new_predictions[, 1])

# VISUALIZING PREDICTIONS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# View new data predictions
new_predictions[1:10, ]

# Convert to data frame and rename variables
new_predictions <- as.data.frame(new_predictions)
colnames(new_predictions) <- c("Adele", "Taylor Swift", "Beyoncé")

# Create tidy data structure
plot_posterior <- gather(new_predictions, key = "artist_name", value = "predict")

# Print formated data
head(plot_posterior)

# Create plot of 
ggplot(plot_posterior, aes(x = predict)) +
  facet_wrap(~ artist_name, ncol = 1) +
  geom_density()