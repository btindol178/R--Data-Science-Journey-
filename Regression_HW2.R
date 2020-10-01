# Blake Tindol HW2 Script
####################################################################################################################################################################
####################################################################################################################################################################
rm(list=ls())
library(ggplot2)

setwd('C:/Users/blake/OneDrive/WMU FALL 2020/Regression/HW2')

df <- read.csv("muscle_mass.csv")# Muscle Mass dataframe
####################################################################################################################################################################
####################################################################################################################################################################
####################################################################################################################################################################
# HW Questions
# 1a) Plot a scatterplot of age on the horizontal axis versus mass on the vertical axis. Do you
     # think that a simple linear regression model is appropriate for this data
plot(df$age,df$mass,ylab = "Mass",xlab = "Age",main = "Muscle Mass")
# a) Answer: Yes a simple linear regression is appropriate for the data because even though it is a negative relationship the relationship is linear

####################################################################################################################################################################
#1b) Does the scatterplot in part (a) support the anticipation that muscle mass decreases with
    #age? Briefly explain.
   # b) Answer: Yes by looking at the scatter plot there is a negative linear relationship between Mass and Age. 
         # If you also look at the linear regression model estimates from the model lm(df$age ~ df$mass, data = df) then you will see mass has a coefficent of -0.63031 meaning for every unit increase in age mass decreases by this amount??
####################################################################################################################################################################
#1c) Provide estimates of the slope and the intercept for the regression of mass on age.
fit <- lm(df$mass ~ df$age, data = df)
coef(fit) # estimates of slope and intercept
# 1c) Answer: The intercept is 156.346 and 
#  a point estimate of the difference in the mean muscle mass for women differing in age by one year
    #is the slope, which is the coefficient listed in the "Age" row slope is -1.1896 (does this make sense)

plot(df$age,df$mass,ylab = "Mass",xlab = "Age",main = "Muscle Mass")
abline(lm(df$mass ~ df$age, data = df), col = "blue")
####################################################################################################################################################################
#1d (d) What is an estimate of the mean muscle mass for women aged 60 years? Compute the
    #value of the residual for this observation
# A point estimate of the mean muscle mass for women aged X = 60 years can be computed:
YI = 156.34656 - 1.19*60
YI

# You can also use predict method
# Dont use df$mass in lm when data =df is already there makes vector of results
fit <- lm(mass ~ age, data = df)
new.data = data.frame(age = 60)
newpred <- predict(fit, newdata =new.data)
newpred
###############################################################################################################################################################################
#1e) What is the estimate of simga squared (Answer in lec3 in week 5 presentation)
summary(fit) # look at residual squared error is sigma

# the estimation of simga is 8.173 and so simga^2 is
 sigma2 = 8.137^2
 sigma2 # answer

 # You can also find it a point estimate sigma 2 is MSE = 66.8, read from the "Residual" row and MS column in the anova table in the output.
 sigma2 = anova(fit)
 sigma2[3] # 88.8 is the MSE RESIDUAL
 ###############################################################################################################################################################################
 #1f)  Recall that residual is defined as ei = yi -yhati. Prove that sumni=1 = 0
 sum(fit$residuals) # essentially zero
 df$yi <- 156.346564 + -1.189996 *df$age # get yi
 df$e <- df$mass - df$yi # e = y - yi
sum(df$e) # Essentially 0

 # (g) Now compute Pn i=1  ei for this dataset. Is it equal to zero in accord with part (f
sum(df$e) # Essentially 0
# or
sum(fit$residuals) 
 ###############################################################################################################################################################################
 ###############################################################################################################################################################################
 ###############################################################################################################################################################################
 # Question 2
sales <- read.csv("salestraining.csv");colnames(sales)[1] <- "Trainee"
 ###############################################################################################################################################################################
#2a) Identify the predictor and the response
  # 2a) Answer: The predictor IS Days of training and the response is sales performance score
 ###############################################################################################################################################################################
 #2b)Plot the scatterplot of days training versus sale performance score. Do you think a simple linear regression model is appropriate for this data? Briefly explain. 
  plot(sales$Days.of.training,sales$Sale.performance.score,ylab = "Performance",xlab = "Training",main = "Performance vs Training")
 abline(lm(Sale.performance.score ~ Days.of.training, data = sales), col = "blue")
  #2b) Answer: No a simple linear regression is not appropriate for this because even though the independent variable is numeric it is displayed like a factor on the x axis
        # This is more of a binomial regression problem
 ###############################################################################################################################################################################
 #2c) Create a new predictor which is the square root transformation of the sale performance score (e.g. for the first trainee, the transformation is  )
 sales$sqrtperf <- sqrt(sales$Sale.performance.score)
 ###############################################################################################################################################################################
 #2d) Plot the scatter plot of days of training versus  a new predictor in part (c). Do you think that a simple linear regression model is appropriate for this data? Explain
 plot(sales$Days.of.training,sales$sqrtperf,ylab = "Square Root of Performance",xlab = "Training",main = "Performance vs Training")
 abline(lm(sqrtperf ~ Days.of.training, data = sales), col = "blue")
 
 #2d) Answer: No this is still a binomial regression problem nothing has fundamentally changed
 ###############################################################################################################################################################################
 ###############################################################################################################################################################################
 ###############################################################################################################################################################################
 #3) What is a relationship between b0 and b0new. Provide a LS estimate of new b0new
 # Create a new dataframe
 df2 <- df
 
 x = df2$age; y = df2$mass
 x.bar = mean(x); y.bar = mean(y);
 xx.bar = x-x.bar
 yy.bar = y-y.bar
 xy.bar = xx.bar * yy.bar
 tableLS = matrix(0,nrow(sales),7)
 tableLS = cbind(x,y,xx.bar,yy.bar,xy.bar,xx.bar^2,yy.bar^2)
 colnames(tableLS)= c("x","y","xx.bar","yy.bar","xy.bar","xx.bar2","yy.bar2")
tableLS[1:5,] 

# b1 estimation
beta1.hat = sum(xx.bar*yy.bar)/sum(xx.bar^2)
beta1.hat
beta0.hat = y.bar - beta1.hat*x.bar
beta0.hat
c(beta0.hat = beta0.hat, beta1.hat = beta1.hat)

# doesnt make sense because (xi-xbar) means that every value would be the error 
df2$normal_yi <- 156.346564  + -1.189996 *df2$age 
df2$e <- df2$mass - df2$normal_yi
df2$oldequation <- 156.346564  + -1.189996 *df2$age + df2$e

xbar = mean(df2$age)
xdif <- df2$age - xbar
df2$newequation <-  156.346564  + -1.189996 * xdif + df2$e

xnewbar <- mean(xdif)
# New estimate of b0new?
beta0new <-y.bar- beta1.hat*xnewbar # do i need to do this or no?


###############################################################################################################################################################################
###############################################################################################################################################################################
################# `##############################################################################################################################################################
#4) Question 4 Simulated data

#4a & b) Simulated data function

SimulateData <- function(n,x,beta0,beta1,error){
  y = beta0 + beta1*x + error
  return(data.frame(x=x,y=y))
}

df2 = NULL;

for(i in 1:10000){
  n = 100
  x = rnorm(n,0,1)
  beta0 = 0.1
  beta1 = 0.5
  error = rnorm(n,0,1)
  data.sample = SimulateData(n,x,beta0,beta1,error)
  beta1.vec= lm(y~x, data = data.sample)#$coefficient[2]
  beta1.vec =beta1.vec$coefficient[2]
  beta1.vec <- unname(beta1.vec);  #beta1.vec <- as.numeric(beta1.vec)
  df2$beta1.vec[i] <- beta1.vec
}

hist(df2$beta1.vec, main = "Beta1 Distribution", xlab = "Beta1")
###############################################################################################################################################################################
# #4c) Simulate histogram
# b1plot<- SimulatedData(10000,20)
# # Hist of b1
# hist(b1plot$beta1.sampling,xlab = "Beta1",main="Beta1.Sampling")
###############################################################################################################################################################################
#4d) Mean and standard deviation of b1
mean(b1plot$beta1.sampling)
sd(b1plot$beta1.sampling)

# Answer: The result makes sense? smean is about exactly 0 and standard deviation is what it is????????????????? 