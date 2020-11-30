# HW5 Regression 

# Rewatch lecture 12 and 13
##################################################################################################################################################################################################################################################
rm(list=ls())
setwd("C:/Users/blake/OneDrive/WMU FALL 2020/Regression/HW5")

install.packages("ALSM")
library(ALSM)
data("GroceryRetailer")
head(GroceryRetailer)
#############################################################################################
# Prep for questions

fit1 <- lm(y ~ x1 + x2 + x3, data = GroceryRetailer)
# Summary fitted values and residuals
summary(fit1); fit1$fitted.values[1:5]; fit1$residuals[1:5]
plot(fit1)# Given the residuals vs leverage we see observations 43 ,16,32 are ones to observe first

# Fitted vs Residuals
plot(fit1$fitted.values, fit1$residuals, xlab = 'Fitted values',
     ylab='Residuals', cex.lab=1.5, cex.axis=1.5)

# X1 residuals vs values
plot(GroceryRetailer$x1, fit1$residuals, xlab = 'x1',
     ylab='Residuals', cex.lab=1.5, cex.axis=1.5)
plot(GroceryRetailer$x2, fit1$residuals, xlab = 'x1',
     ylab='Residuals', cex.lab=1.5, cex.axis=1.5)

# qq norm
plot(fit1, which = c(2))

######################################################################################################
# Question 1A)Studentized residuals
#Obtain the studentized deleted residuals and identify any outlying y observations. Use
#the case ID to identify them so we know which cases you have identified.
##################################
#first obtain the standard MSE
s = summary(fit1)$sigma
residuals(fit1)[1:5]/s #Just first 5 (semistudentized residuals)
residuals(fit1)/s      # all semistudentized residuals

# completely studentized residuals
rstandard(fit1)[1:5] #first 5
rstandard(fit1) # all
#vec1 <- data.frame(y =GroceryRetailer$y, residual = rstandard(fit1))
#vec1$absdelres <- abs(vec1$residual)
#vec2 <- vec1[order(-vec1$absdelres),];vec1

# Studentized Deleted Residuals
rstudent(fit1)[1:5] # first 5
rstudent(fit1) # all
vec1 <- data.frame(y =GroceryRetailer$y, delresidual = rstudent(fit1))
vec1$absdelres <- abs(vec1$delresidual)
vec1[order(-vec1$absdelres),]# no values over 3......


# 1A) Answer!!!!!!!!!!!!!!!!!!!!!
# Because there is no studentized deleted residuals over 3 there is nothing to look into but we will look into 
# 40,38,10,32 34


##################################################################################################################
# Question 1B 
#Obtain the diagonal element of the hat matrix. Identify and outlying x observations using
#the rule of thumb discussed in class. Again use the case ID to identify them so we know
#which cases you have identified.

lev = round(hatvalues(fit1),3);lev
vec1$lev <- lev;vec1

sum(hatvalues(fit1))

which(lev >= (2*3)/30)

vec1[order(-vec1$lev),][1:5,]

# We have identified potential outlying x observations: (43,48,22,16,44) # they are the top 5

############################################################################################
# Question 1C
# Suppose that a manger wishes to predict the total labor hours required to handle the
# next shipment containing Cases = 300,000 cases whose indirect costs of the total house
# is Costs = 7.2 and Holiday = 0, i.e. no holiday in week. Provide a scatter plot of Cases
# against Costs and determine visually whether this prediction involves an extrapolation
# beyond the range of the data.
plot(GroceryRetailer$x1,GroceryRetailer$x2)

new <- data.frame(x1=300000, x2=7.2,x3=0)
predict(fit1,new)

#I would say that there are visually this prediction fits into the model and does not involve extrapolation beyond range of data  
###############################################################################################
# Question 1D
# Obtain DFFITS and Cook's distance values for each of the outlying cases in part (a) and
#(b). What do these measures indicate about the influence of each of the cases?

## Calculate DFFITS
ddfits1 <- dffits(fit1)
vec1$ddfits <- ddfits1;vec1
vec1[order(-vec1$absdelres),][1:5,]# order them in order of deleted studentized residuals in part a when discouvered for part a
vec1[order(-vec1$lev),][1:5,] # for part b order them like this

# These observations could be potentially seen as influential because they may cause major changes in fitted regression

## Calculate Cook's distance
cook1 <- cooks.distance(fit1)
vec1$cook <- cook1;vec1
vec1[order(-vec1$absdelres),][1:5,] # for part a order them like this
vec1[order(-vec1$lev),][1:5,] # for part b order them like this

################################################################################################
# 1(e) Obtain Cook's distance for each case. Are any cases influential according to this measure?
## Calculate Cook's distance
## Using DFFITS to indentify influential observations
which(abs(dffits(fit1)) > 2) # no particular observation is significant

## Use Cook's distance to indentify influential observations
which(cooks.distance(fit1) > qf(.2, df1=4, df2=30-4))

# Using cooks distance for each case it does not look like there is any influential cases
###################################################################################################
###################################################################################################
# Question 2
#Consider the influence of parental genetics on stature in Problem set 4 using the data set
# childHeight.txt.
childHeight <- read.delim("childHeight.txt")

# 2a) Fit the a regression model by using all predictor variables. Show the summary and a
#fitting regression line.

fit2 <- lm(height ~ exercise + male + dadht + momht, data = childHeight)
summary(fit2) 
fit2$fitted.values
plot(fit2)

# equation is (height ~ 16.98645 + -0.0052*exercise + 5.311*male + 0.4127*dadht + 2.991*momht)


# 2 b) Based on the result in part (a), discuss why you decided to keep or drop variables.
# Answer: Based on summary exercise does not seem to be significant meaning no difference from 0 and has potential to be dropped from the model

#2 c)  Conduct a F-test to determine whether the predictors in part (b) can be dropped from the
# regression model. Carefully state the null and alternative, test statistics and conclusion.
# 
# n = nrow(childHeight)
# # define vector of response and design matrix
# Y = as.matrix(childHeight$height,ncol=1)
# X = matrix(1,nrow=n,ncol=n)
# X  = cbind(rep(1,nrow(childHeight)), childHeight$alcohol,childHeight$alcohol,childHeight$male,childHeight$dadht,childHeight$momht)
# b = solve(t(X)%*%X)%*%t(X)%*%Y
# 
# # Define matrices I, J and H
# I = diag(1,n)
# J = matrix(1,n,n)
# H = X %*% solve(t(X) %*% X) %*% t(X)
# 
# #LS estimators
# beta.hat = solve(t(X)%*%X)%*%t(X)%*%Y
# 
# # SSTO 
# SSTO = t(Y)%*%Y - (1/n)*t(Y)%*%J%*%Y
# SSTO
# 
# #SSR 
# SSR = t(beta.hat)%*%t(X)%*%Y - (1/n)*t(Y)%*%J%*%Y
# SSR
# 
# #SSE
# SSE = t(Y)%*%Y - t(beta.hat)%*%t(X)%*%Y
# SSE

fit2 <- lm(height ~ exercise + male + dadht + momht, data = childHeight)
anova(fit2)
sse =anova(fit2)[5,2];sse# SSE
ssr =sum(anova(fit2)[1:4,2]);ssr#SSR
ssto = sum(anova(fit2)[1:5,2]);ssto

# #R squared
# rsquared = 1 -(sse/ssto);rsquared
# c(R.square=rsquared, lm=summary(fit2)$r.squared)
# 
# # Adj R squared changed n-number of predictiors-1 ( 4 predictors)
# R.square.adj = 1 - ((n-1)/(n-4-1))*sse/ssto
# c(R.square.adj=R.square.adj, lm=summary(fit2)$adj.r.squared)
# 
# #Fstar
# F.star = (ssr/2)/(sse/(n-4-1))
# F.star # 155.2 
# 
# qf(1-0.05, 4, n-4-1) #F.star = 83.89
# # because f star 83 is is greater than 2.42  we reject and conclude that logFertility are related to
# #logPPgdp and Purban
# 
# pf(F.star, 2, n-2-1, lower.tail = FALSE) #p-values

# for each model we assess sse
mdl1 <- lm(height ~ exercise, data = childHeight)
mdl2 <- lm(height ~ dadht, data = childHeight)
mdl3 <- lm(height ~ momht, data = childHeight)
mdl4 <- lm(height ~ male, data = childHeight)

mdl1.sse =anova(mdl1)[2,2];mdl1.sse
mdl2.sse =anova(mdl2)[2,2];mdl2.sse
mdl3.sse =anova(mdl3)[2,2];mdl3.sse
mdl4.sse =anova(mdl4)[2,2];mdl4.sse 

# full model
full.mdl <- lm(height ~ exercise + male + dadht + momht, data = childHeight)
# reduced model
reduced.mdl1 <- lm(height ~ exercise, data = childHeight);summary(reduced.mdl1)# not significant this variable will be dropped and be the reduced model
reduced.mdl2 <- lm(height ~ dadht , data = childHeight);summary(reduced.mdl2)# significant
reduced.mdl3 <- lm(height ~ momht, data = childHeight);summary(reduced.mdl3)# significant
reduced.mdl4 <- lm(height ~ male, data = childHeight);summary(reduced.mdl4) # significant

# Comparison of models why is it not registering
anova(reduced.mdl1, full.mdl)
# because of a significant p value we would reject the null hypothesis and keep exercise in the model?

# null hypotesis is that exercise is not significant than 0 meaning we do elimenate it from the model and 
# the alternative hypotiseis is that exercsie is signiicant and we reject the null hypothesis and keep it in the model.

#2 d) From part (b) and (c), fit a regression model by using predictors which you decided to
# keep. Call this model Model D
model_d <- lm(height ~ exercise + male + dadht + momht, data = childHeight)

# 2e) Obtain the studentized deleted residuals, the diagonal element of the hat matrix to identify
#any outlying observations. Choose an outlying case and explain why this case was flagged
#as unusual.
# Outliing y
## Studentized deleted residuals
del <- rstudent(model_d)
err <- model_d$residuals
vec <- data.frame(del = del,err=err );vec
vec2<- vec[which(abs(err) > 3),];vec2
vec2$absdel <- abs(vec2$del);vec2
vec2[order(-vec2$absdel),]
#observation 130 is an outlier because the absolute value of the studentized deleted residual is greater than 3

# Outling x obeservations!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
# Hii in matrix h
lev = round(hatvalues(model_d),3);lev
# lev <- data.frame(lev); lev <- sort(lev$lev, decreasing = TRUE);lev
# vec <- data.frame(del = del,err=err);vec
# vec$absdel <- abs(vec$del);vec
# vec[order(-vec$absdel),]
# vec4 <- vec[-c(164),];vec4
sum(hatvalues(model_d))
which(lev >= (2*5)/165) # these larger values indicate that the value is distant from the ith observation
#observations (20,21,77,81,86,99,122,130,136,137)
outliers <- c('20','21','77','81','86','99','122','130','136','137')
vec[outliers,] # filter for the values that are outliers

#(f) Obtain DFFITS and Cook's distance values for each of the outlying cases in part (e).#What do these measures indicate about the influence of each of the cases?
## Calculate DFFITS
dff <- dffits(model_d)
## Calculate Cook's distance
cook <- cooks.distance(model_d)
vec3 <- data.frame(dff = dff,cook=cook);vec3
vec3[outliers,] # The  values that are the outliers that i picked
vec2[order(-vec2$cook),] # largest cook
vec2[order(-vec2$dff),]  # largest ddff


## Using DFFITS to indentify influential observations
which(abs(dffits(model_d)) > 1)

## Use Cook's distance to indentify influential observations
which(cooks.distance(fit2) > qf(.2, df1=3, df2=165-3))

# These cases might be influential in impacting the regression 

#(g) Discuss whether any of the identified cases in part (e) and (f) should be removed from the analysis.
# Looks like the case 130 should be removed from the analysis because it does affect the regression analysis


#(h) Find the Variance Inflation Factor (VIF) for each of the predictors in Model D. Based on
#the these VIF values, what do you suggest about what predictor variable(s) to include in
#your model?
cor(childHeight[2:7],use = "complete.obs")
vif(model_d) # no vif is greater than 10 meaning there is no indicator of multicorilinearity 
