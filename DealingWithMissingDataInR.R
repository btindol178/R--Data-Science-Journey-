# Missing Data 
setwd("C:/Users/blake/OneDrive/GITHUBJUPITERPROJECT")
library(ggplot2)
library(dplyr)
#install.packages("naniar",lib = "C:/R/R-4.0.2/library")
library(naniar) # missing data package
#install.packages("rwalkr",lib = "C:/R/R-4.0.2/library")
library(rwalkr)
library(datasets)
library(lubridate)
library(tidyr)
library(tidyverse)
library(broom)
library(purrr)
data("pedestrian")
##############################################################################################################
##############################################################################################################
# missing 
# is there any NA
x <- c(1,NA,3,NA,NA,5)
any_na(x)

# are these values na
are_na(x)

# number of missing
n_miss(x)

# proportion_missing
prop_miss(x)

# working with NA
heights <- data.frame(names = c("Sophie","Dan","Fred"),height = c(165,177,NA))
sum(heights$height) # cant sum a column with an NA value

# Types of missing
any_nan(NaN) # Not a number such as square root of -1
any_null(NULL) # empty but not missing
any_na(Inf) # inf value from things like 10/0 notmissing

# Create x, a vector, with values NA, NaN, Inf, ".", and "missing"
x <- c(NA, NaN, Inf, ".", "missing")

# Use any_na() and are_na() on to explore the missings
any_na(x)
are_na(x)


# Making missing data
df<-data.frame(height=rnorm(100,50,15),
               weight=rnorm(100,22,5),
               width=rnorm(100,17,12),
               intelligence=rnorm(100,35,7),
               smell=rnorm(100,82,20))

insert_nas <- function(x) {
  len <- length(x)
  n <- sample(1:floor(0.2*len), 1)
  i <- sample(1:len, n)
  x[i] <- NA 
  x
}


insert_nas <- function(x) {
  len <- length(x)
  n <- sample(1:floor(0.2*len), 1)
  i <- sample(1:len, n)
  if(n <= 5){
    x[i] <- Inf 
  } else if(n > 5 & n <= 10){
    x[i] <- NaN
  } else if(n > 10 & n <=15){
    x[i] <- NA
  } else {
    x[i] <- "missing"
  }
  x
}
dat_hw <- sapply(df, insert_nas)
dat_hw <- data.frame(dat_hw)

# Use n_miss() to count the total number of missing values in dat_hw
n_miss(dat_hw)

# Use n_miss() on dat_hw$weight to count the total number of missing values
n_miss(dat_hw$weight)

# Use n_complete() on dat_hw to count the total number of complete values
n_complete(dat_hw)

# Use n_complete() on dat_hw$weight to count the total number of complete values
n_complete(dat_hw$weight)

# Use prop_miss() and prop_complete on dat_hw to count the total number of missing values in each of the variables
prop_miss(dat_hw)
prop_complete(dat_hw)

# How to summarize missing values
#summary of every NA values in the dataframe for each column
miss_var_summary(dat_hw)

# represents each row or case of dataframe
miss_case_summary(dat_hw)

# Give tabulations number and percentage
miss_var_table(dat_hw) # reads 1 variable with 1 missing , 1 variable with 5 missing, 1 with 13, 1 with 15 and one with 19

# same thing for cases
miss_case_table(dat_hw)

# retrieve time series data for pedestrians
pedestrian <- melb_walk_fast(year =2017)
pedestrian

# missing data summaries for time series
#calculates the number of missing in a variable for a repeating span
miss_var_span(pedestrian,var = Count,span_every = 4000)

# Miss var run returns the run or streaks of missingness
miss_var_run(pedestrian,Count)

# install airqualitydataset
data(airquality)

# using summaries by group
airquality %>%
  group_by(Month)%>%
  miss_var_summary()

# missing cases
miss_case_summary(airquality)


# Return the summary of missingness in each variable, 
# grouped by Month, in the `airquality` dataset
airquality %>% group_by(Month) %>% miss_var_summary()

# Return the summary of missingness in each case, 
# grouped by Month, in the `airquality` dataset
airquality %>% group_by(Month) %>% miss_case_summary()


# Tabulate missingness in each variable and case of the `airquality` dataset
miss_var_table(airquality)
miss_case_table(airquality)

# Tabulate the missingness in each variable, grouped by Month, in the `airquality` dataset
airquality %>% group_by(Month) %>% miss_var_table()

# Tabulate of missingness in each case, grouped by Month, in the `airquality` dataset
airquality %>% group_by(Month) %>% miss_case_table()

# Calculate the summaries for each run of missingness for the variable, hourly_counts
miss_var_run(pedestrian, var = hourly_counts)

# Calculate the summaries for each span of missingness, 
# for a span of 4000, for the variable hourly_counts
miss_var_span(pedestrian, var = hourly_counts, span_every = 4000)

# For each `month` variable, calculate the run of missingness for hourly_counts
pedestrian %>% group_by(month) %>% miss_var_run(var = hourly_counts)

# For each `month` variable, calculate the span of missingness 
# of a span of 2000, for the variable hourly_counts
pedestrian %>% group_by(month) %>% miss_var_span(var = hourly_counts, span_every = 2000)

# Visualize missing values
vis_miss(airquality)
vis_miss(airquality,cluster =TRUE)

# Visualize another way
gg_miss_var(airquality)
gg_miss_case(airquality)

#faceting by month
gg_miss_var(airquality,facet = Month)

# number of combination of missing values that co-occur
gg_miss_upset(airquality) # this one says there is only missing in solar and ozone with 35 in only ozone, 5 in only solar.r and 2 in both at same time

# factor of missingness
gg_miss_fct(x=airquality,fct = Month)

#ggmis span
gg_miss_span(pedestrian, Count,span_every=20000)

# load new data
data("riskfactors")

# Visualize all of the missingness in the `riskfactors`  dataset
vis_miss(riskfactors)

# Visualize and cluster all of the missingness in the `riskfactors` dataset
vis_miss(riskfactors, cluster = TRUE)

# Visualize and sort the columns by missingness in the `riskfactors` dataset
vis_miss(riskfactors, sort_miss = TRUE)

# Visualize the number of missings in cases using `gg_miss_case()`
gg_miss_case(riskfactors)

# Explore the number of missings in cases using `gg_miss_case()` 
# and facet by the variable `education`
gg_miss_case(riskfactors, facet = education)

# Visualize the number of missings in variables using `gg_miss_var()`
gg_miss_var(riskfactors)

# Explore the number of missings in variables using `gg_miss_var()` 
# and facet by the variable `education`
gg_miss_var(riskfactors, facet = education)

# Using the airquality dataset, explore the missingness pattern using gg_miss_upset()
gg_miss_upset(airquality)

# With the riskfactors dataset, explore how the missingness changes across the marital variable using gg_miss_fct()
gg_miss_fct(x = riskfactors, fct = marital)

# Using the pedestrian dataset, explore how the missingness of hourly_counts changes over a span of 3000 
gg_miss_span(pedestrian, var = hourly_counts, span_every = 3000)

# Using the pedestrian dataset, explore the impact of month by faceting by month
# and explore how missingness changes for a span of 1000
gg_miss_span(pedestrian, var = hourly_counts, span_every = 20000, facet = month)

##########################################################################################################
# Searching for and replacing missing values
miss_scan_count()
pedestrian %>%
  miss_scan_count(search = list("N/A"))

# Replace "N/A" AND "N/a" 
pedestrian %>%
  replace_with_na(search = list("N/A","N/a"))

# replace all -99
pedestrian %>%
  replace_with_na_all(condition == -99 )

# Dont have pacman dataset
pacman <- dat_hw
# Explore the strange missing values "N/A"
miss_scan_count(data = pacman, search = list("N/A"))

# Explore the strange missing values "missing"
miss_scan_count(data = pacman, search = list("missing"))

# Explore the strange missing values "na"
miss_scan_count(data = pacman, search = list("na"))

# Explore the strange missing values " " (a single space)
miss_scan_count(data = pacman, search = list(" "))

# Explore all of the strange missing values, "N/A", "missing", "na", " "
miss_scan_count(data = pacman, search = list("N/A", "missing","na", " "))

# Use `replace_with_na_at()` to replace with NA
replace_with_na_at(pacman,
                   .vars = c("year", "month", "day"), 
                   ~.x %in% c("N/A", "missing", "na", " "))

# Use `replace_with_na_if()` to replace with NA the character values using `is.character`
replace_with_na_if(pacman,
                   .predicate = is.character, 
                   ~.x %in% c("N/A", "missing", "na", " "))

# Use `replace_with_na_all()` to replace with NA
replace_with_na_all(pacman, ~.x %in% c("N/A", "missing", "na", " "))

# missing implicit missing 
# Print the frogger data to have a look at it
frogger

# Use `complete()` on the `time` and `name` variables to  
# make implicit missing values explicit
frogger_tidy <- frogger %>% complete(time, name)

# Print the frogger data to have a look at it
frogger

# Use `fill()` to fill down the name variable in the frogger dataset
frogger %>% fill(name)

# Print the frogger data to have a look at it
frogger

# Correctly fill() and complete() missing values so that our dataset becomes sensible
frogger %>%
  fill(name) %>%
  complete(name, time)

oceanbouys <- naniar::oceanbuoys
# Arrange by year
oceanbuoys %>% arrange(year) %>% vis_miss()

# Arrange by latitude
oceanbuoys %>% arrange(latitude) %>% vis_miss()

# Arrange by wind_ew (wind east west)
oceanbuoys %>% arrange(wind_ew) %>% vis_miss()

# `bind_shadow()` and `group_by()` humidity missingness (`humidity_NA`)
oceanbuoys %>%
  bind_shadow() %>%
  group_by(humidity_NA) %>%
  summarize(wind_ew_mean = mean(wind_ew), # calculate mean of wind_ew
            wind_ew_sd = sd(wind_ew)) # calculate standard deviation of wind_ew

# Repeat this, but calculating summaries for wind north south (`wind_ns`)
oceanbuoys %>%
  bind_shadow() %>%
  group_by(humidity_NA) %>%
  summarize(wind_ns_mean = mean(wind_ns),
            wind_ns_sd = sd(wind_ns))

# Summarize wind_ew by the missingness of `air_temp_c_NA`
oceanbuoys %>% 
  bind_shadow() %>%
  group_by(air_temp_c_NA) %>%
  summarize(wind_ew_mean = mean(wind_ew),
            wind_ew_sd = sd(wind_ew),
            n_obs = n())

# Summarize wind_ew by missingness of `air_temp_c_NA` and `humidity_NA`
oceanbuoys %>% 
  bind_shadow() %>%
  group_by(air_temp_c_NA, humidity_NA) %>%
  summarize(wind_ew_mean = mean(wind_ew),
            wind_ew_sd = sd(wind_ew),
            n_obs = n())

airquality %>%
  bind_shadow() %>%
  ggplot(aes(x=Temp,
             color = Ozone_NA)) + 
  geom_density()

airquality %>%
  bind_shadow() %>%
  ggplot(aes(x=Ozone_NA,
             color = Temp)) + 
  geom_boxplot()

airquality %>%
  bind_shadow() %>%
  ggplot(aes(x = Temp)) +
  geom_density() +
  facet_wrap(~Ozone_NA)

airquality %>%
  bind_shadow() %>%
  ggplot(aes(x = Temp, y = Wind)) +
  geom_point() +
  facet_wrap(~ Ozone_NA)
  
airquality %>%
  bind_shadow() %>%
  ggplot(aes(x = Temp,
             y = Wind,
             color = Ozone_NA)) +
  geom_point() 

airquality %>%
  bind_shadow() %>%
  ggplot(aes(x = Temp,
             color = Ozone_NA)) +
  geom_density() + 
  facet_wrap(~ Solar.R_NA)

# First explore the missingness structure of `oceanbuoys` using `vis_miss()`
vis_miss(oceanbuoys)

# Explore the distribution of `wind_ew` for the missingness  
# of `air_temp_c_NA` using  `geom_density()`
bind_shadow(oceanbuoys) %>%
  ggplot(aes(x = wind_ew, 
             color = air_temp_c_NA)) + 
  geom_density()

# Explore the distribution of sea temperature for the  
# missingness of humidity (humidity_NA) using  `geom_density()`
bind_shadow(oceanbuoys) %>%
  ggplot(aes(x = sea_temp_c,
             color = humidity_NA)) + 
  geom_density()


# Explore the distribution of wind east west (wind_ew) for the missingness of air temperature 
# using geom_density() and faceting by the missingness of air temperature (air_temp_c_NA).
oceanbuoys %>%
  bind_shadow() %>%
  ggplot(aes(x = wind_ew)) + 
  geom_density() + 
  facet_wrap(~air_temp_c_NA)

# Build upon this visualization by filling by the missingness of humidity (humidity_NA).
oceanbuoys %>%
  bind_shadow() %>%
  ggplot(aes(x = wind_ew,
             color = humidity_NA)) + 
  geom_density() + 
  facet_wrap(~air_temp_c_NA)

# Explore the distribution of wind east west (`wind_ew`) for  
# the missingness of air temperature using  `geom_boxplot()`
oceanbuoys %>%
  bind_shadow() %>%
  ggplot(aes(x = air_temp_c_NA,
             y = wind_ew)) + 
  geom_boxplot()

# Build upon this visualization by faceting by the missingness of humidity (`humidity_NA`).
oceanbuoys %>%
  bind_shadow() %>%
  ggplot(aes(x = air_temp_c_NA,
             y = wind_ew)) + 
  geom_boxplot() + 
  facet_wrap(~humidity_NA)

# visualize missing in scatterplot
ggplot(airquality,
       aes(x=Ozone,
           y = Solar.R)) +
  geom_point()

ggplot(airquality,
       aes(x=Ozone,
           y = Solar.R)) +
  geom_miss_point()

ggplot(airquality,
       aes(x=Wind,
           y = Ozone)) +
  geom_miss_point() +
  facet_wrap(~ Month)

airquality %>%
  bind_shadow()%>%
ggplot(aes(x=Wind,
           y = Ozone)) +
  geom_miss_point() +
  facet_wrap(~ Solar.R_NA)

# Explore the missingness in wind and air temperature, and  
# display the missingness using `geom_miss_point()`
ggplot(oceanbuoys,
       aes(x = wind_ew,
           y = air_temp_c)) + 
  geom_miss_point()

# Explore the missingness in humidity and air temperature,  
# and display the missingness using `geom_miss_point()`
ggplot(oceanbuoys,
       aes(x = humidity,
           y = air_temp_c)) + 
  geom_miss_point()

# Explore the missingness in wind and air temperature, and display the 
# missingness using `geom_miss_point()`. Facet by year to explore this further.
ggplot(oceanbuoys,
       aes(x = wind_ew,
           y = air_temp_c)) + 
  geom_miss_point() + 
  facet_wrap(~year)

# Explore the missingness in humidity and air temperature, and display the 
# missingness using `geom_miss_point()` Facet by year to explore this further.
ggplot(oceanbuoys,
       aes(x = humidity,
           y = air_temp_c)) + 
  geom_miss_point() + 
  facet_wrap(~year)

# Use geom_miss_point() and facet_wrap to explore how the missingness 
# in wind_ew and air_temp_c is different for missingness of humidity
bind_shadow(oceanbuoys) %>%
  ggplot(aes(x = wind_ew,
             y = air_temp_c)) + 
  geom_miss_point() + 
  facet_wrap(~humidity_NA)

# Use geom_miss_point() and facet_grid to explore how the missingness in wind_ew and air_temp_c 
# is different for missingness of humidity AND by year - by using `facet_grid(humidity_NA ~ year)`
bind_shadow(oceanbuoys) %>%
  ggplot(aes(x = wind_ew,
             y = air_temp_c)) + 
  geom_miss_point() + 
  facet_grid(humidity_NA~year)

##################################################################################################################
##################################################################################################################
# Imputation
aq_imp <- airquality %>%
  bind_shadow() %>%
  impute_below_all()

ggplot(aq_imp,
       aes(x = Ozone,
           fill = Ozone_NA))+
  geom_histogram()

ggplot(aq_imp,
       aes(x = Ozone,
           fill = Ozone_NA))+
  geom_histogram() +
  facet_wrap(~ Month)

ggplot(aq_imp,
       aes(x = Ozone,
           fill = Ozone_NA))+
  geom_histogram() +
  facet_wrap(~ Solar.R_NA)

aq_imp <- airquality %>%
  bind_shadow() %>%
  add_label_shadow() %>%
  impute_below_all()

ggplot(aq_imp,
       aes(x = Ozone,
           y = Solar.R,
           color = any_missing)) +
  geom_point()

# Impute the data below the range using `impute_below`.
ocean_imp <- impute_below_all(oceanbuoys)

# Visualize the new missing values
ggplot(ocean_imp,
       aes(x = wind_ew, y = air_temp_c)) + 
  geom_point()

# Impute and track data with `bind_shadow`, `impute_below`, and `add_label_shadow`
ocean_imp_track <- bind_shadow(oceanbuoys) %>% 
  impute_below_all() %>%
  add_label_shadow()

# Look at the imputed values
ocean_imp_track


# Impute and track the missing values
ocean_imp_track <- bind_shadow(oceanbuoys) %>% 
  impute_below_all() %>%
  add_label_shadow()

# Visualize the missingness in wind and air temperature,  
# coloring missing air temp values with air_temp_c_NA
ggplot(ocean_imp_track,
       aes(x = wind_ew, y = air_temp_c, color = air_temp_c_NA)) + 
  geom_point()

# Visualize humidity and air temp, coloring any missing cases using the variable any_missing
ggplot(ocean_imp_track,
       aes(x = humidity, y = air_temp_c, color = any_missing)) + 
  geom_point()

# Explore the values of air_temp_c, visualizing the amount of missings with `air_temp_c_NA`.
p <- ggplot(ocean_imp_track, aes(x = air_temp_c, fill = air_temp_c_NA)) + geom_histogram()

# Expore the missings in humidity using humidity_NA
p2 <- ggplot(ocean_imp_track, aes(x = humidity, fill = humidity_NA)) + geom_histogram()

# Explore the missings in air_temp_c according to year, using `facet_wrap(~year)`.
p + facet_wrap(~year)

# Explore the missings in humidity according to year, using `facet_wrap(~year)`.
p2 + facet_wrap(~year)

# track missing values
aq_impute_mean <- airquality %>%
  bind_shadow(only_miss = TRUE) %>%
  impute_mean_all() %>%
  add_label_shadow()
aq_impute_mean

ggplot(aq_impute_mean,
       aes(x = Ozone_NA,
           y = Ozone)) +
  geom_boxplot()

ggplot(aq_impute_mean,
       aes(x = Ozone,
           y = Solar.R,
           color = any_missing)) +
  geom_point()

aq_imp <- airquality %>%
  bind_shadow() %>%
  impute_mean_all()

aq_imp_long <- shadow_long(aq_imp,
                           Ozone,
                           Solar.R)
aq_imp_long

ggplot(aq_imp_long,
       aes(x=value,
           fill=value_NA)) +
  geom_histogram() +
  facet_wrap(~variable)

# Impute the mean value and track the imputations 
ocean_imp_mean <- bind_shadow(oceanbuoys) %>% 
  impute_mean_all() %>% 
  add_label_shadow()

# Explore the mean values in humidity in the imputed dataset
ggplot(ocean_imp_mean, 
       aes(x = humidity_NA, y = humidity)) + 
  geom_boxplot()

# Explore the values in air temperature in the imputed dataset
ggplot(ocean_imp_mean, 
       aes(x = air_temp_c_NA, y = air_temp_c)) + 
  geom_boxplot()

# Explore imputations in air temperature and humidity,  
# coloring by the variable, any_missing
ggplot(ocean_imp_mean, 
       aes(x = air_temp_c, y = humidity, color = any_missing)) + 
  geom_point()

# Explore imputations in air temperature and humidity,  
# coloring by the variable, any_missing, and faceting by year
ggplot(ocean_imp_mean, 
       aes(x = air_temp_c, y = humidity, color = any_missing)) +
  geom_point() + 
  facet_wrap(~year)

# Gather the imputed data 
ocean_imp_mean_gather <- shadow_long(ocean_imp_mean,
                                     humidity,
                                     air_temp_c)
# Inspect the data
ocean_imp_mean_gather

# Explore the imputations in a histogram 
ggplot(ocean_imp_mean_gather, 
       aes(x = value, fill = value_NA)) + 
  geom_histogram() + 
  facet_wrap(~variable)

#############################################################################################
# Imputtation using linear models
install.packages("simputation",lib = "C:/R/R-4.0.2/library")
library(simputation)

dat_hw %>%
  bind_shadow(only_miss = TRUE) %>%
  add_label_shadow() %>%
  impute_lm(height ~ weight + width + intelligence + smell)

aq_imp_lm <- airquality %>% bind_shadow() %>% add_label_shadow() %>%
  impute_lm(Solar.R ~ Wind + Temp + Month) %>%
  impute_lm(Ozone ~ Wind + Temp + Month)
  aq_imp_lm

  # Impute humidity and air temperature using wind_ew and wind_ns, and track missing values
  ocean_imp_lm_wind <- oceanbuoys %>% 
    bind_shadow() %>%
    impute_lm(air_temp_c ~ wind_ew + wind_ns) %>% 
    impute_lm(humidity ~ wind_ew + wind_ns) %>%
    add_label_shadow()
  
  # Plot the imputed values for air_temp_c and humidity, colored by missingness
  ggplot(ocean_imp_lm_wind, 
         aes(x = air_temp_c, y = humidity, color = any_missing)) +
    geom_point()

  # Bind the models together 
  bound_models <- bind_rows(mean = ocean_imp_mean,
                            lm_wind = ocean_imp_lm_wind,
                            .id = "imp_model")
  
  # Inspect the values of air_temp and humidity as a scatter plot
  ggplot(bound_models,
         aes(x = air_temp_c,
             y = humidity,
             color = any_missing)) +
    geom_point() + 
    facet_wrap(~imp_model)
  
  # Build a model adding year to the outcome
  ocean_imp_lm_wind_year <- bind_shadow(oceanbuoys) %>%
    impute_lm(air_temp_c ~ wind_ew + wind_ns + year) %>%
    impute_lm(humidity ~ wind_ew + wind_ns + year) %>%
    add_label_shadow()
  
  # Bind the mean, lm_wind, and lm_wind_year models together
  bound_models <- bind_rows(mean = ocean_imp_mean,
                            lm_wind = ocean_imp_lm_wind,
                            lm_wind_year = ocean_imp_lm_wind_year,
                            .id = "imp_model")
  
  # Explore air_temp and humidity, coloring by any missings, and faceting by imputation model
  ggplot(bound_models, aes(x = air_temp_c, y = humidity, color = any_missing)) +
    geom_point() + facet_wrap(~imp_model)
  
  
#1. Complete cases
  aq_cc <- airquality %>%
    na.omit()%>%
    bind_shadow() %>%
    add_label_shadow()
  
# Imputation using the imputed data from the last lesson
  aq_imp_lm <- bind_shadow(airquality)%>%
    add_label_shadow() %>%
    impute_lm(Ozone ~ Temp + Wind + Month + Day) %>%
    impute_lm(Solar.R ~ Temp + Wind + Month + Day) 
    
# 3. Bind the models together
  bound_models <- bind_rows(cc = aq_cc,
                            imp_lm = aq_imp_lm,
                            .id = "imp_model")
  
  # Create an imputed dataset using a linear models
  ocean_imp_lm_all <- bind_shadow(oceanbuoys) %>%
    add_label_shadow() %>%
    impute_lm(sea_temp_c ~ wind_ew + wind_ns + year + latitude + longitude) %>%
    impute_lm(air_temp_c ~ wind_ew + wind_ns + year + latitude + longitude) %>%
    impute_lm(humidity ~ wind_ew + wind_ns + year + latitude + longitude)
  
  # Bind the datasets
  bound_models <- bind_rows(cc = ocean_cc,
                            imp_lm_wind = ocean_imp_lm_wind,
                            imp_lm_all = ocean_imp_lm_all,
                            .id = "imp_model")
  
  # Look at the models
  bound_models
  

  # Create the model summary for each dataset
  model_summary <- bound_models %>% 
    group_by(imp_model) %>%
    nest() %>%
    mutate(mod = map(data, ~lm(sea_temp_c ~ air_temp_c + humidity + year, data = .)),
           res = map(mod, residuals),
           pred = map(mod, predict),
           tidy = map(mod, tidy))
  
  # Explore the coefficients in the model
  model_summary %>% 
    select(imp_model,tidy) %>%
    unnest()
  best_model <- "imp_lm_all"