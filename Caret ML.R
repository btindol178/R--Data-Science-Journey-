# Here i found an advertising dataset that will fit good for regression
install.packages("caret")
library(caret)

df <- read.csv("Advertising.csv")
summary(df) # no NA values

# Using Caret for Regression to test insample vs out of sample rmse to reduce overfitting 
#Here we will will make a model

# just get rid of the first column record id
df <- df[-1] # just get rid of the first column record id

# Regression 
mdl <- lm(Sales ~., df)

# Predict using full data
pred <- predict(mdl, df)

# Compute errors: error
error <- pred - df[["Sales"]]

# Calculate RMSE
sqrt(mean(error ^ 2))

#Out of sample error 
#First lets split our data
set.seed(34)

# Shuffle row indices:rows
rows <- sample(nrow(df))

#Randomly order data
shuffled_df <- df[rows, ]

# Determine row to split on: split
split <- round(nrow(df) * 0.80) # use 80% of data for training

# Create train
train <- df[1:split, ] # 1:80% of dataset or row 160 

# Create test
test <- df[(split + 1):nrow(df), ] # take the rest of the rows in the df that is not test

# Fit lm model on train: model
model <- lm(Sales ~ ., train)

# Predict on test: p
p <- predict(model, test)

# Compute errors: error
error <- p - test[["Sales"]]

# Calculate RMSE
sqrt(mean(error^2))

# Now fit the linear regression using 10-fold Cross Validation instead
# Fit lm model using 10-fold CV: model
# we will need caret for this 

install.packages("caret")
library(caret)

model <- train(
  Sales ~ ., 
  df,
  method = "lm",
  trControl = trainControl(
    method = "cv", 
    number = 10,
    verboseIter = TRUE
  )
)

# Print model to console
model

# Fit lm model using 5-fold CV: model to see difference
model2 <- train(
  Sales ~ ., 
  df,
  method = "lm",
  trControl = trainControl(
    method = "cv", 
    number = 5,
    verboseIter = TRUE
  )
)

# Print model to console
model2 #Slightly higher rmse

# Fit lm model using 5 x 5-fold CV: model
model3 <- train(
  Sales ~ ., 
  df,
  method = "lm",
  trControl = trainControl(
    method = "repeatedcv", 
    number = 5,
    repeats = 5, #this allows you to repeat the 5 fold 5 times
    verboseIter = TRUE
  )
)

# Print model to console
model3 #Slightly higher again

# Predict on full advertising dataset using 10 fold cross validation
p2 <- predict(model,test)

# Compute errors: error
error2 <- p2 - test[["Sales"]]

# Calculate RMSE
sqrt(mean(error^2)) # just about the same lower than train!!!

# LOGISTIC REGRESSION (CLASSIFICATION)
# lets change the sales to a binary value for logistic regression!
range(df$Sales) # 1.6 to 27.0
mean(df$Sales) # 14.025

# Make column if sales are over 14 then it is 1 ("high sales") and less than 14 it is 0 
df$Sales2 <- ifelse(df$Sales< 14,0,1);

# Make df that doesnt include original sales column
df_c <- df[-4] # get rid of 4th column

# Get the number of observations
n_obs <- nrow(df_c)

# Shuffle row indices: permuted_rows
permuted_rows <- sample(n_obs)

# Randomly order data: df_c
df_c_shuffled <- df_c[permuted_rows, ]

# Identify row to split on: split
split <- round(n_obs * 0.6)

# Create train
train <- df_c_shuffled[1:split, ]

# Create test
test <- df_c_shuffled[(split + 1):n_obs, ]

# Fit glm model: model
model <- glm(Sales2 ~ ., family = "binomial", train)
summary(model)

# Predict on test: p
p <- predict(model, test, type = "response")

# If p exceeds threshold of 0.5, High (1) else (0) Low: H_or_L
# get middle threshold
mean(p)
h_or_l <- ifelse(p > 0.5, 1, 0)
h_or_l <- as.factor(h_or_l) # making it a factor
test$sales2 <- as.factor(test$Sales2) # making it factor

# Create confusion matrix must have the two comparison columns same level and type (this case factor)
confusionMatrix(h_or_l, test$sales2) #92% accuracy pretty good

# Using different classification thrushold
# 10% would catch more high sales with less certainty 
# 90% would catch less high sales with more certainty

# If p exceeds threshold of 0.9
h_or_l <- ifelse(p > 0.9, 1, 0)
h_or_l <- as.factor(h_or_l) # making it a factor

# Create confusion matrix 90% now
#For example, pretend you want to identify the objects you are really certain are highsales. In this case, you might want to use a probability threshold of 
confusionMatrix(h_or_l, test$sales2)  # only 90% accuracy 
#sensitivity is the ability of a test to correctly identify those that are high sales (true positive rate)
# less accuracy now but higher certainty or (sensitivity)

# If p exceeds threshold of 0.1
# we want to be really certain that model correctly predicts all high sales
h_or_l <- ifelse(p > 0.1, 1, 0)
h_or_l <- as.factor(h_or_l) # making it a factor

# Create confusion matrix 10% now
confusionMatrix(h_or_l, test$sales2) #95% accuracy pretty good
#specificity is the ability of the test to correctly identify those that are not high sales (true negative rate).
# higher accuracy now but make sure predicted is correct (higher specificity)

#USING ROC CURVE
install.packages("caTools")
library(caTools)

# Fit glm model: model
model <- glm(Sales2 ~ ., family = "binomial", train)
summary(model)

# Predict on test: p
p <- predict(model, test, type = "response")


# Make ROC curve #99% accuracy 
colAUC(p, test$Sales2, plotROC = TRUE)

# AUC Curve (area under curve) perfect score is 1

# Create trainControl object: myControl
myControl <- trainControl(
  method = "cv",
  number = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE
)

# Train glm with custom trainControl: model
# must make outcome column a factor for classification first
df_c <- df[-4] # get rid of 4th column

# this doesnt work 0 and 1 are not proper classes
#first convert column to factor for next step 
df_c$Sales2 <- as.factor(df_c$Sales2) 

#do this once column 0 and 1 converted to factor 
levels(df_c$Sales2) <- c("first_class","second_class")

model <- train(
  Sales2 ~ ., 
  df_c, 
  method = "glm",
  trControl = myControl
)

# Print model to console
model
