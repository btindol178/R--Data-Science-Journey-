# Experimental Design T-test,Anova,and other methods
###############################################################################################################
rm(list=ls())
setwd("C:/Users/blake/OneDrive/GITHUBJUPITERPROJECT")
NYCSATScores <- read.csv("NYCSATScores.csv")
NHANESMedicalConditions <- read.csv('NHANESMedicalConditions.csv')
NHANESFinalCombined <- read.csv('NHANESFinalCombined.csv')
NHANESDemographics <- read.csv('NHANESDemographics.csv')
NHANESBodyMeasures <- read.csv('NHANESBodyMeasures.csv')
lendingclub <- read.csv("lendingclubdata.csv"); colnames(lendingclub)[1] <- "member_id"
###############################################################################################################
###############################################################################################################
# Experimental Design
#Planning-Design- Testing
#randomization(fightaginst biased)-replication and blocking

#ToothGrowth is a built-in R dataset from a study that examined the effect of three different doses of Vitamin C on the length of the odontoplasts, the cells responsible for teeth growth in 60 guinea pigs, where tooth length was the measured outcome variable.
data("ToothGrowth");ToothGrowth <- data.frame(ToothGrowth)

# Perform a two-sided t-test
t.test(x = ToothGrowth$len, alternative = "two.sided", mu = 18)
#Excellent job! Given the high p-value, we fail to reject the null hypothesis that the mean of len is equal to 18. That is, we don't have evidence that it is different from 18 micrometers. P-values and hypothesis testing will be covered in more detail in the next video

#In the experiment that yielded the ToothGrowth dataset, guinea pigs were randomized to receive Vitamin C either through orange juice or ascorbic acid, indicated in the dataset by the supp variable. It's natural to wonder if there is a difference in tooth length by supplement type - a question that a t-test can also answer!
# Perform a t-test
ToothGrowth_ttest <- t.test(len ~ supp, data = ToothGrowth)
# Load broom
library(broom)
# Tidy ToothGrowth_ttest
tidy(ToothGrowth_ttest)

#Nice job! Given the p-value of around 0.06, there seems to be no evidence to support the hypothesis that there's a difference in mean tooth length by supplement type, or, more simply, that there is no difference in mean tooth length by supplement type. Generally in most experiments, any p-value above 0.05 will offer no evidence to support the given hypothesis.

#############################################################
#Replication
#Recall that replication means you need to conduct an experiment with an adequate number of subjects to achieve an acceptable statistical power. Sample size and power will be discussed in more detail later in this chapter.
#Let's examine the ToothGrowth dataset to make sure they followed the principle of replication. We'll use dplyr to do some exploratory data analysis (EDA). The data has already been loaded for you.
# Load dplyr
library(dplyr)

# Count number of observations for each combination of supp and dose
ToothGrowth %>% 
  count(supp, dose)
# The researchers seem to have tested each combination of supp and dose on 10 subjects each, which is low, but was deemed adequate for this experiment

#########################################################################################
#Blocking
#Though this is not true, suppose the supplement type is actually a nuisance factor we'd like to control for by blocking, and we're actually only interested in the effect of the dose of Vitamin C on guinea pig tooth growth.
#If we block by supplement type, we create groups that are more similar, in that they'll have the same supplement type, allowing us to examine only the effect of dose on tooth length.

library(ggplot2)

# Create a boxplot with geom_boxplot()
ggplot(ToothGrowth, aes(x = dose, y = len)) + 
  geom_boxplot()

# Create ToothGrowth_aov
ToothGrowth_aov <- aov(len ~ dose + supp, data = ToothGrowth)

# Examine ToothGrowth_aov with summary()
summary(ToothGrowth_aov)

#You have just designed your first Randomized Complete Block Design (RCBD) experiment. We'll learn more about this type of experiment in Chapter 3. Given the very small observed p-value for dose, it appears we have evidence to support the hypothesis that mean len is different by dose amount.

#############################################################################################################
#Hypothesis Testing
# Null hypothesis is there is no change, no difference between groups
# alternative hypothesis, there is a change, 
# if testing is less than or greater than that is one sided hypothesis
# if testing that observation is NOT equal to a number than that is a two sided hypothesis

# Power is the probability that the test correctly rejects the null hypothesis when the alternative hypothesis is true
# Goal is to have 80% power in your experiments

# Effect size: standardized measure of the difference you are trying to detect
# difference in grouped means by pooled standard deviaiton of data

# Sample size: how many experimental units you need to survey to detect the desired difference at the desired power

######################
#power package for an anova
#install.packages('pwr',lib = "C:/R/R-4.0.2/library")
library(pwr)

# One must be entered as null to be calculated
pwr.anova.test(k=3, # number of  groups
               n=20, #  number of observations per group
               f=0.2, # effect size
               sig.level = 0.05, # significance level 
               power = NULL)  # power
# The power is 0.252


################################################
#Recall in the first exercise that we tested to see if the mean of the guinea pigs' teeth in ToothGrowth was not equal to 18 micrometers. That was an example of a two-sided t-test: we wanted to see if the mean of len is some other number on either side of 18.
#We can also conduct a one-sided t-test, explicitly checking to see if the mean is less than or greater than 18. Whether to use a one- or two-sided test usually follows from your research question. Does an intervention cause longer tooth growth? One-sided, greater than. Does a drug cause the test group to lose more weight? One-sided, less than. Is there a difference in mean test scores between two groups of students? 

# Less than
t.test(x = ToothGrowth$len,
       alternative = "less",
       mu = 18)
#p-value = 0.7933

# Greater than
t.test(x = ToothGrowth$len,
       alternative = "greater",
       mu = 18)
# p-value = 0.2067
# It turns out the mean of len is actually very close to 18, so neither of these tests tells us much about the mean of tooth length.

# Calculate power
#Calculate power using an effect size of 0.35, a sample size of 100 in each group, and a significance level of 0.10.
pwr.t.test(n = 100,
           d = 0.35,# effect size
           sig.level = 0.1,
           type = "two.sample",
           alternative = "two.sided",
           power = NULL)

#Calculate the sample size needed with an effect size of 0.25, a significance level of 0.05, and a power of 0.8.
# Calculate sample size
pwr.t.test(n = NULL,
           d = 0.25, # effect size
           sig.level = 0.05, 
           type = "one.sample", 
           alternative = "greater", 
           power = 0.8)
# Because this sample size calculation was for a one-sided test, we only need 101 subjects, not 101 in each group. As you design experiments in the future, the pwr package includes functions for calculating power and sample size for a variety of different tests, including ANOVA (more on that in the next chapter!

###############################################################
# ANOVA single and multiple and single factor
# we can only know if one of the means is different than the other and not know which one

# Examine the variables with glimpse()
glimpse(lendingclub)


# Find median loan_amnt, mean int_rate, and mean annual_inc with summarize()
lendingclub %>% summarize(median(loan_amnt), 
                          mean(int_rate), 
                          mean(annual_inc))

# Use ggplot2 to build a bar chart of purpose
ggplot(data=lendingclub, aes(x = purpose)) + 
  geom_bar() +
  coord_flip()

# Use recode() to create the new purpose_recode variable
lendingclub$purpose_recode <- lendingclub$purpose %>% recode( 
  "credit_card" = "debt_related", 
  "debt_consolidation" = "debt_related",
  "medical" = "debt_related",
  "car" = "big_purchase", 
  "major_purchase" = "big_purchase", 
  "vacation" = "big_purchase",
  "moving" = "life_change", 
  "small_business" = "life_change", 
  "wedding" = "life_change",
  "house" = "home_related", 
  "home_improvement" = "home_related")

#########################################
# SINGLE FACTOR EXPERIMENTS

#How does loan purpose affect amount funded?
#In the last exercise, we pared the purpose variable down to a more reasonable 4 categories and called it purpose_recode. As a data scientist at Lending Club, we might want to design an experiment where we examine how the loan purpose influences the amount funded, which is the money actually issued to the applicant.

#Use lm() to look at how the purpose_recode variable affects funded_amnt. Save the model as an object called purpose_recode_model
# Build a linear regression model, purpose_recode_model
purpose_recode_model <- lm(funded_amnt ~ purpose_recode, data = lendingclub)

# Examine results of purpose_recode_model
summary(purpose_recode_model)

# Get anova results and save as purpose_recode_anova
purpose_recode_anova <- anova(purpose_recode_model)

# Print purpose_recode_anova
purpose_recode_anova

# Examine class of purpose_recode_anova
class(purpose_recode_anova)

#Based on the very small p-value, purpose_recode_anova's results indicate that there is evidence to support the hypothesis that the mean loan amounts are different for at least one combination of purpose_recode's levels. You also saw that the ANOVA results are saved as a data frame, which is nice in case you need to access results later. Loans aren't issued in a vacuum, however, and it's likely that more than just purpose influences the amount funded.

##Which loan purpose mean is different?
#  Before we examine other factors besides purpose_recode that might influence the amount of loan funded, let's examine which means of purpose_recode are different. This is the post-hoc test referred to in the last exercise.
#The result of that ANOVA test was statistically significant with a very low p-value. This means we can reject the null hypothesis and accept the alternative hypothesis that at least one mean was different. But which one?
#We should use Tukey's HSD test, which stands for Honest Significant Difference. To conduct Tukey's HSD test in R, you can use TukeyHSD():

#uild a model using aov() that examines funded_amnt by purpose_recode. Save it as purpose_aov.
#Use TukeyHSD() to conduct the Tukey's HSD test on purpose_aov with a confidence level of 0.95. Save as an object called tukey_output.

# Use aov() to build purpose_aov
purpose_aov <- aov(funded_amnt ~ purpose_recode, data = lendingclub)

# Conduct Tukey's HSD test to create tukey_output
tukey_output <- TukeyHSD(purpose_aov, "purpose_recode", conf.level = 0.95)

# Tidy tukey_output to make sense of the results
tidy(tukey_output)
# Looking at the p-values for each comparison of the levels of purpose_recode, we can see that only a few of the mean differences are statistically significant, for example the differences in the means for the debt_related and big_purchase loan amounts. In this case, these tiny p-values are most likely to be due to large sample size, and further tests would be required to determine what's actually significant in the case of loans (known as the practical significance.)

###################################
# Multiple Factor Experiments
#Use aov() to build a linear model and ANOVA in one step, examining how purpose_recode and employment length (emp_length) affect the funded amount. Save as an object purpose_emp_aov and print the result out.

# Use aov() to build purpose_emp_aov
purpose_emp_aov <- aov(funded_amnt ~ purpose_recode + emp_length, data = lendingclub)

# Print purpose_emp_aov to the console
purpose_emp_aov

# Call summary() to see the p-values
summary(purpose_emp_aov)
#You could also perform Tukey's HSD test on this model, but given that emp_length has 12 levels, it'll be quite the output. If it was important to the experiment to know the answer, you'd definitely need to look at it.

tukey_output2 <- TukeyHSD(purpose_aov, 'emp_length' , conf.level = 0.95)
tidy(tukey_output2)

########################################################################################
########################################################################################
# Model validation

# Examine the summary of int_rate
summary(lendingclub$int_rate)

# Examine int_rate by grade
lendingclub %>% 
  group_by(grade) %>% 
  summarize(mean = mean(int_rate), var = var(int_rate), median = median(int_rate))

# Make a boxplot of int_rate by grade
ggplot(lendingclub, aes(x = grade, y = int_rate)) + 
  geom_boxplot()

# Use aov() to create grade_aov plus call summary() to print results
grade_aov <- aov(int_rate ~ grade, data = lendingclub)
summary(grade_aov)
#You can see from the numeric summary and the boxplot that grade seems to heavily influence interest rate. Therefore, the linear model results indicating that int_rate is significantly different by grade are unsurprising.

# Another assumption of ANOVA and linear modeling is homogeneity of variance. Homogeneity means "same", and here that would mean that the variance of int_rate is the same for each level of grade. We can test for homogeneity of variances using bartlett.test(), which takes a formula and a dataset as inputs.
# For a 2x2 grid of plots:
par(mfrow = c(2, 2))

# Plot grade_aov
plot(grade_aov)

# Bartlett's test for homogeneity of variance
bartlett.test(int_rate ~ grade, data = lendingclub)
#The residuals on this model are okay, though the residuals on G have a much smaller range than any other level of grade (the dots are far less spread out.) The Q-Q plot, however, shows that the residuals are fairly normal. However, given the highly significant p-value from Bartlett's test, the assumption of homogeneity of variances is violated, which is one of the assumptions of an ANOVA model. Therefore, ANOVA might not be the best choice for this experiment. It happens!

#One non-parametric alternative to ANOVA is the Kruskal-Wallis rank sum test. For those with some statistics knowledge, it is an extension of the Mann-Whitney U test for when there are more than two groups, like with our grade variable. For us, the null hypothesis for this test would be that all of the int_rates have the same ranking by grade.
#The Kruskal-Wallis rank sum test can be conducted using the kruskal.test() function, available in base R. Luckily for you, the use of this function is very similar to using lm() or aov(): you input a formula and a dataset, and a result is returned.

#Use kruskal.test() to examine whether int_rate varies by grade when a non-parametric model is employed.

# Conduct the Kruskal-Wallis rank sum test
kruskal.test(int_rate ~ grade,
             data = lendingclub)
#The low p-value indicates that based on this test, we can be confident in our result, which we found across this experiment, that int_rate varies by grade.

###########################################################################################################################################################################
# A/B testing
#Sample size for A/B test
##We know now that we need to analyze our A/B test results with a t-test after we've collected data. We have two pretty important questions we need to answer before we do that: what's the effect size and what's the sample size required for this test?
#In this case, effect size was given to us. Lending Club is looking to detect the relatively small effect size of 0.2. We'll again use the pwr package and calculate sample size using an appropriate function to find out how many we'll need to recruit into each group, A and B.
#Use the correct function from the pwr package to calculate the required sample size for each group with d = 0.2, a power of 0.8, and a 0.05 significance level. Check the pwr help docs with ?pwr if you need help remembering which function to use and what arguments it takes.

# Use the correct function from pwr to find the sample size
pwr.t.test(n = NULL, 
           d = 0.2,
           sig.level = 0.05,
           power = 0.8,
           alternative = "two.sided")
#Nice! You can see we need about 400 people per group to reach our desired power in this A/B test.

#Basic A/B test
#Now that we know the sample size required, and we allowed the experiment to run long enough to get at least 400 people in each group, we can analyze our A/B test.
#Remember that when applicants were using the Lending Club website, they were randomly assigned to two groups, A or B, where A was shown a mint green website header and B was shown a light blue website header. Lending Club was interested to see if website header color choice influenced loan_amnt, the amount an applicant asked to borrow.
# Plot the A/B test results
ggplot(lendingclub, aes(x = Group, y = loan_amnt)) + 
  geom_boxplot()

# Conduct a two-sided t-test
t.test(loan_amnt ~ Group, data = lendingclub_ab)

# Build lendingclub_multi
lendingclub_multi <- lm(loan_amnt ~  grade + verification_status, data = lendingclub)

# Examine lendingclub_multi results
tidy(lendingclub_multi)
#From the results, verification status and having an F grade are the factors in this model that have a significant effect on loan amount. Let's move on to the next chapter and conduct more multivariable experiments like this.

#########################################################################################################################################################################################
#Sampling

#Fill in and run the dplyr code to find mean weight (bmxwt) in kg by our treatment (mcq365d). Is there anything interesting about the NA treated patients?
# Fill in the dplyr code
NHANESFinalCombined %>% 
  group_by(mcq365d) %>% 
  summarize(mean = mean(bmxwt, na.rm = TRUE))

# Fill in the ggplot2 code
NHANESFinalCombined %>% 
  ggplot(aes(as.factor(mcq365d), bmxwt)) +
  geom_boxplot() +
  labs(x = "Treatment",
       y = "Weight")

# Filter to keep only those 16+
nhanes_filter <- NHANESFinalCombined %>% filter(ridageyr > 16)

# Load simputation & impute bmxwt by riagendr
library(simputation)

nhanes_final <- impute_median(nhanes_filter, bmxwt ~ riagendr)

# Recode mcq365d with recode() & examine with count()
nhanes_final$mcq365d <- recode(nhanes_final$mcq365d, 
                               `1` = 1,
                               `2` = 2,
                               `9` = 2)
nhanes_final %>% count(mcq365d)
#Imputation is a powerful tool for dealing with missing data, but should be used with caution, as you can introduce bias into your data if you're not careful how you impute. Now that we have the dataset cleaned, we're ready to learn about RCBDs so we can analyze our experiment.

#Create nhanes_stratified by using group_by() and sample_n(). Stratify by riagendr and select 2000 of each gender. Confirm that it worked by using count() to examine nhanes_stratified's gender variable.
# Use sample_n() to create nhanes_srs
nhanes_srs <- nhanes_final %>% sample_n(2500)

# Create nhanes_stratified with group_by() and sample_n()
nhanes_stratified <- nhanes_final %>% group_by(riagendr) %>% sample_n(2000)
nhanes_stratified %>% 
  count(riagendr)

# Load sampling package and create nhanes_cluster with cluster()
#install.packages("sampling",lib = "C:/R/R-4.0.2/library")
library(sampling)
nhanes_cluster <- cluster(nhanes_final, "indhhin2", 6, method = "srswor")

###########################
#Randomized complete block design
# Create designs using ls()
install.packages("agricolae",lib = "C:/R/R-4.0.2/library")
library(agricolae)

designs <- ls("package:agricolae", pattern = "design")
print(designs)

# Use str() to view design.rcbd's criteria
str(design.rcbd)

# Build treats and rep
treats <- LETTERS[1:5]
blocks <- 4


# Build my_design_rcbd and view the sketch
my_design_rcbd <- design.rcbd(treats, r = blocks, seed = 42)
my_design_rcbd$sketch

#################
nhanes_final <- NHANESFinalCombined

# Use aov() to create nhanes_rcbd
nhanes_rcbd <- aov(bmxwt ~ mcq365d + riagendr, data = nhanes_final)

# Check results of nhanes_rcbd with summary()
summary(nhanes_rcbd)

# Print mean weights by mcq365d and riagendr
nhanes_final %>% 
  group_by(mcq365d, riagendr) %>% 
  summarize(mean_wt = mean(bmxwt, na.rm = TRUE))
#It's pretty clear that there truly is a mean difference in weight by gender, so blocking was a good call for this experiment. We also observed a statistically significant effect of the treatment on bmxwt, which we hope is actually a result of the treatment. Now that we have the RCBD down, let's tackle Balanced Incomplete Block Designs (BIBD).

# Set up the 2x2 plotting grid and plot nhanes_rcbd
par(mfrow = c(2, 2))
plot(nhanes_rcbd)

# Run the code to view the interaction plots
with(nhanes_final, interaction.plot(riagendr, mcq365d, bmxwt))


# Balanced incomeplete block design BIBD
#create my_design_bibd_1
my_design_bibd_1 <- design.bib(LETTERS[1:3], k = 4, seed = 42)

#create my_design_bibd_2
my_design_bibd_2 <- design.bib(LETTERS[1:8], k = 3, seed = 42)

#create my_design_bibd_3
my_design_bibd_3 <- design.bib(LETTERS[1:4], k = 4, seed = 42)
my_design_bibd_3$sketch

# Calculate lambda
lambda(4, 3, 3)

# Build the data.frame
creatinine <- c(1.98, 1.97, 2.35, 2.09, 1.87, 1.95, 2.08, 2.01, 1.84, 2.06, 1.97, 2.22)
food <- as.factor(c("A", "C", "D", "A", "B", "C", "B", "C", "D", "A", "B", "D"))
color <- as.factor(rep(c("Black", "White", "Orange", "Spotted"), each = 3))
cat_experiment <- as.data.frame(cbind(creatinine, food, color))

# Create cat_model and examine with summary()
cat_model <- aov(creatinine ~ food + color, data = cat_experiment)
summary(cat_model)

# Calculate lambda
lambda(3, 2, 2)

# Create weightlift_model & examine results
weightlift_model <- aov(bmxarmc ~ weightlift_treat + ridreth1, data = nhanes_final)
summary(weightlift_model)

##################################################################################################
#Latin Squares
nyc_scores <- NYCSATScores

# Mean, var, and median of Math score by Borough
nyc_scores %>%
  group_by(Borough) %>% 
  summarize(mean = mean(Average_Score_SAT_Math, na.rm = TRUE),
            var = var(Average_Score_SAT_Math, na.rm = TRUE),
            median = median(Average_Score_SAT_Math, na.rm = TRUE))

# Mean, var, and median of Math score by Teacher Education Level
nyc_scores %>%
  group_by(Teacher_Education_Level) %>% 
  summarize(mean = mean(Average_Score_SAT_Math, na.rm = TRUE),
            var = var(Average_Score_SAT_Math, na.rm = TRUE),
            median = median(Average_Score_SAT_Math, na.rm = TRUE))

# Mean, var, and median of Math score by both
nyc_scores %>%
  group_by(Borough, Teacher_Education_Level) %>% 
  summarize(mean = mean(Average_Score_SAT_Math, na.rm = TRUE),
            var = var(Average_Score_SAT_Math, na.rm = TRUE),
            median = median(Average_Score_SAT_Math, na.rm = TRUE))

# Load naniar
library(naniar)

# Examine missingness with miss_var_summary()
nyc_scores %>% miss_var_summary()

# Examine missingness with md.pattern()
md.pattern(nyc_scores)

# Impute the Math score by Borough
nyc_scores_2 <- impute_median(nyc_scores, Average_Score_SAT_Math ~ Borough)


# Convert Math score to numeric
nyc_scores_2$Average_Score_SAT_Math <- as.numeric(nyc_scores_2$Average_Score_SAT_Math)

# Load agricolae
library(agricolae)

# Design a LS with 5 treatments A:E then look at the sketch
my_design_lsd <- design.lsd(trt = LETTERS[1:5], seed = 42)
my_design_lsd$sketch

# Build nyc_scores_ls_lm
nyc_scores_ls_lm <- lm(Average_Score_SAT_Math ~ Tutoring_Program + Borough + Teacher_Education_Level,
                       data = nyc_scores_ls)

# Tidy the results with broom
tidy(nyc_scores_ls_lm)

# Examine the results with anova
anova(nyc_scores_ls_lm)

# Create a boxplot of Math scores by Borough, with a title and x/y axis labels
ggplot(nyc_scores, aes(Borough, Average_Score_SAT_Math)) +
  geom_boxplot() + 
  labs(title = "Average SAT Math Scores by Borough, NYC",
       x = "Borough (NYC)",
       y = "Average SAT Math Score (2014-15)")

# Create trt1 and trt2
trt1 <- LETTERS[1:5]
trt2 <- 1:5

# Create my_graeco_design
my_graeco_design <- design.graeco(trt1, trt2, seed = 42)

# Examine the parameters and sketch
my_graeco_design$parameters
my_graeco_design$sketch

# Build nyc_scores_gls_lm
nyc_scores_gls_lm <- lm(Average_Score_SAT_Math ~ Tutoring_Program + Borough + Teacher_Education_Level + Homework_Type,
                        data = nyc_scores_gls)

# Tidy the results with broom
tidy(nyc_scores_gls_lm)

# Examine the results with anova
anova(nyc_scores_gls_lm)

# Load ggplot2
library(ggplot2)

# Build the boxplot for the tutoring program vs. Math SAT score
ggplot(nyc_scores,
       aes(Tutoring_Program, Average_Score_SAT_Math)) + 
  geom_boxplot()


# Build the boxplot for percent tested vs. Math SAT score
ggplot(nyc_scores,
       aes(Percent_Tested_HL, Average_Score_SAT_Math)) + 
  geom_boxplot()

# Create nyc_scores_factorial and examine the results
nyc_scores_factorial <- aov(Average_Score_SAT_Math ~ Percent_Tested_HL * Percent_Black_HL * Tutoring_Program, data = nyc_scores)

tidy(nyc_scores_factorial)

# Use shapiro.test() to test the outcome
shapiro.test(nyc_scores$Average_Score_SAT_Math)

# Plot nyc_scores_factorial to examine residuals
par(mfrow = c(2,2))
plot(nyc_scores_factorial)