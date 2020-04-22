df <- read.csv("Advertising1.csv")
df2 <- read.csv("sugarsample.csv")

head(df,n=10)

df$highlow <- ifelse(df$Sales>13,1,0)

head(df,n=5)

summary(df)
str(df)

install.packages("ggplot2")
library(ggplot2)

#ONE DIMENSIONS DECISION BOUNDTRY USING MARGIN MAXIMIZATION USING SVM
# Plot sugar content along the x-axis
plot_df <- ggplot(data = df2, aes(x = sugar_content, y = 0)) + 
  geom_point() + 
  geom_text(aes(label = sugar_content), size = 2.5, vjust = 2, hjust = 0.5)

# Display plot
plot_df

#The maximal margin separator is at the midpoint of the two extreme points in each cluster.
mm_separator <- (8.9+10)/2

#create data frame containing the maximum margin separator
separator <- data.frame(sep = mm_separator)

#add separator to sugar content scatterplot
plot_sep <- plot_df + geom_point(data = separator, aes(x = mm_separator, y = 0), color = "blue", size = 4)

#display plot
plot_sep

# GETTING TWO DIMENSIONAL DATASET
# CREATE A 2 VARIABLE DATASET WITH 200 POINTS AND UNIFORMLY DISTRIBUTED 
n <- 200
# set seed to ensure reproducibility
set.seed(40)
# generate dataframe with 2 predictors x1 and x2 in (0,1)
df3 <- data.frame(x1 = runif(n),
                  x2 = runif(n))

# classify points as -1 or 1
df3$y <- factor(ifelse(df3$x1-df3$x2>0,-1,1),levels=c(-1,1))

# plot data
p <- ggplot(data=df3, aes(x =x1,y=x2,color=y))+ geom_point() +
  scale_color_manual(values=c("-1" = "red","1" = "blue")) +
  geom_abline(slope=1,intercept=0)
p

# remove values close to decision bountry
#create margin of 0.05 
delta <- 0.05
#retain only those points that lie outside margin from decision bountry(delta away)
df1 <- df3[abs(df3$x1-df3$x2)>delta,]
#check remaining
nrow(df1)

# plot data
p1 <- ggplot(data=df1, aes(x =x1,y=x2,color=y))+ geom_point() +
  scale_color_manual(values=c("-1" = "red","1" = "blue")) +
  geom_abline(slope=1,intercept=0)
p1

#ploting decision bountries
p2 <- p1 + geom_abline(slope=1,intercept=delta,linetype ="dashed") +
  geom_abline(slope=1,intercept=-delta,linetype="dashed")
p2

# Linear Support Vector Machine
# decision bountry is called kernel !!!!!!!
install.packages("e1071")
library(e1071)

# will will use df3 which has 200 rows and normally distributed from 0 to 1
set.seed() = 1
df3[,"train"]<- ifelse(runif(nrow(df3))<0.8,1,0)

#Seperate training and test sets
trainset <- df3[df3$train == 1,]
testset <- df3[df3$train == 0,]
trainColumn <- grep("train", names(trainset))
trainset <- trainset[,-trainColumn]
testset <- testset[,-trainColumn]

# call linear svm
svm_model <-svm(y ~ .,
                data=trainset,
                type="C-classification",
                kernel = "linear",
                scale = FALSE) # only because we made the dataset scaled when we created dataframe 
svm_model
svm_model$inde
# plot model
plot(x = svm_model,data=trainset)

# TUNING THE SVM'S PARAMETERS
#build svm model, cost = 1
svm_model_1 <- svm(y ~ .,
                   data = trainset,
                   type = "C-classification",
                   cost = 1,
                   kernel = "linear",
                   scale = FALSE)

#print model details
svm_model_1

#build svm model, cost = 100
svm_model_100 <- svm(y ~ .,
                     data = trainset,
                     type = "C-classification",
                     cost = 100, # CHANGE COST TO 100
                     kernel = "linear",
                     scale = FALSE)

#print model details
svm_model_100 # 23 SUPPORT VECTORS 


# MULTI CLASS PROBLEMS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
library(datasets)
data(iris)
summary(iris)

# Split dataset
split <- round(nrow(iris) * 0.80)
train <- iris[1:split,]
test <- iris[(split + 1):nrow(iris),]

#load library and build svm model
library(e1071)
svm_model<- 
  svm(Species ~ ., data = train, type = "C-classification", 
      kernel = "linear")
# CALCULATE PERFORMANCE
# ACCURACY!!!!!!!!!!!!!
pred_train <- predict(svm_model,train)
mean(pred_train==train$Species)# accuracy
pred_test <- predict(svm_model,test)
mean(pred_test==test$Species) # ACCURACY ON NEW INTRODUCED DATA
#plot
plot(svm_model,train)


# USING A BETTER METHOD
# build linear SVMs for 100 distinct training/test partitions of the iris dataset. You will then evaluate the performance of your model by calculating 
#the mean accuracy and standard deviation. 
accuracy <- NULL;
for (i in 1:100){ 
  #assign 80% of the data to the training set
  iris[, "train"] <- ifelse(runif(nrow(iris)) < 0.8, 1, 0)
  trainColNum <- grep("train", names(iris))
  trainset <- iris[iris$train == 1, -trainColNum]
  testset <- iris[iris$train == 0, -trainColNum]
  #build model using training data
  svm_model <- svm(Species~ ., data = trainset, 
                   type = "C-classification", kernel = "linear")
  #calculate accuracy on test data
  pred_test <- predict(svm_model, testset)
  accuracy[i] <- mean(pred_test == testset$Species)
}
mean(accuracy)# GET AVERAGE ACCURACY OUT OF 100
sd(accuracy)



# split train and test data in an 80/20 proportion
df[, "train"] <- ifelse(runif(nrow(df))<0.8, 1, 0)

#assign training rows to data frame trainset
trainset <- df[df$train == 1, ]
#assign test rows to data frame testset
testset <- df[df$train == 0, ]

#find index of "train" column
trainColNum <- grep("train", names(df))

#remove "train" column from train and test dataset
trainset <- trainset[, -trainColNum]
testset <- testset[, -trainColNum]

library(e1071)

#build svm model, setting required parameters
svm_model<- svm(y ~ ., 
                data = trainset, 
                type = "C-classification", 
                kernel = "linear", 
                scale = FALSE)


# list of model components
names(svm_model)

#list values of the SV, index and rho
svm_model$SV
svm_model$index
svm_model$rho

#compute training accuracy
pred_train <- predict(svm_model, trainset)
mean(pred_train == trainset$y)

#compute test accuracy
pred_test <- predict(svm_model, testset)
mean(pred_test == testset$y)

# CREATING A RADIALLY SEPERABLE DATASET!!!!!!!!!!!!!!!!!
n <- 400
set.seed(1)

#Generate data frame with two uniformly distributed predictors, x1 and x2
df <- data.frame(x1 = runif(n, min = -1, max = 1), 
                 x2 = runif(n, min = -1, max = 1))

#We want a circular boundary. Set boundary radius 
radius <- 0.8
radius_squared <- radius^2

#create dependent categorical variable, y, with value -1 or 1 depending on whether point lies
#within or outside the circle.
df$y <- factor(ifelse(df$x1^2 + df$x2^2 < radius_squared, -1, 1), levels = c(-1, 1))

#load ggplot
library(ggplot2)

#build scatter plot, distinguish class by color
scatter_plot <- ggplot(data = df, aes(x = x1, y = x2, color = y)) + 
  geom_point() +
  scale_color_manual(values = c("red", "blue"))

#display plot
scatter_plot

# TRY TO CLASSIFY RADIAL DATA WITH LINEAR SVMS (COMPLEX DECISION BOUNTRYS)
# Split dataset
split <- round(nrow(df) * 0.80)
trainset <- df[1:split,]
testset <- df[(split + 1):nrow(df),]

#default cost mode;
svm_model_1 <- svm(y ~ ., data = trainset, 
    type = "C-classification", cost = 1, kernel = "linear")

#training accuracy
pred_train <- predict(svm_model_1, trainset)
mean(pred_train == trainset$y)

#test accuracy
pred_test <- predict(svm_model_1, testset)
mean(pred_test == testset$y)

#cost = 100 model!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
svm_model_2 <- svm(y ~ ., data = trainset, 
      type = "C-classification", cost = 100, kernel = "linear")

#accuracy
pred_train <- predict(svm_model_2, trainset)
mean(pred_train == trainset$y)
pred_test <- predict(svm_model_2, testset)
mean(pred_test == testset$y)

# Print average accuracy and standard deviation
accuracy <- rep(NA, 100)
set.seed(2)

# Calculate accuracies for 100 training/test partitions
for (i in 1:100){
  df[, "train"] <- ifelse(runif(nrow(df)) < 0.8, 1, 0)
  trainset <- df[df$train == 1, ]
  testset <- df[df$train == 0, ]
  trainColNum <- grep("train", names(trainset))
  trainset <- trainset[, -trainColNum]
  testset <- testset[, -trainColNum]
  svm_model <- svm(y ~ ., data = trainset, type = "C-classification", kernel = "linear")
  pred_test <- predict(svm_model, testset)
  accuracy[i] <- mean(pred_test == testset$y)
}

# Print average accuracy and its standard deviation
mean(accuracy)
sd(accuracy)

# Polynomial Kernals!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# kernal functions are generalizations of dot products

# Polynomial model!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
svm_model_3 <- svm(y ~ ., data = trainset, 
                   type = "C-classification", cost = 100, kernel = "polynomial",degree= 2)
#accuracy much better!
pred_train <- predict(svm_model_3, trainset)
mean(pred_train == trainset$y)
pred_test <- predict(svm_model_3, testset)
mean(pred_test == testset$y)
# plot it!
plot(svm_model_3,trainset)

#TUNING SVMS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#Tuning in a nutshell
#How it works: set a range of search values for each parameter. Examples: cost = 10^(-1:3), gamma = c(0.1,1,10), coef0 = c(0.1,1,10)
#tune model
tune_out <- 
  tune.svm(x = trainset[, -3], y = trainset[, 3], 
           type = "C-classification", 
           kernel = "polynomial", degree = 2, cost = 10^(-1:2), 
           gamma = c(0.1, 1, 10), coef0 = c(0.1, 1, 10))

#list optimal values
tune_out$best.parameters$cost
tune_out$best.parameters$gamma
tune_out$best.parameters$coef0

#Build tuned model
svm_model <- svm(y~ ., data = trainset, type = "C-classification", 
                 kernel = "polynomial", degree = 2, 
                 cost = tune_out$best.parameters$cost, 
                 gamma = tune_out$best.parameters$gamma, 
                 coef0 = tune_out$best.parameters$coef0)

#Calculate training and test accuracies   
pred_train <- predict(svm_model, trainset)
mean(pred_train == trainset$y)
pred_test <- predict(svm_model, testset)
mean(pred_test == testset$y)

#plot model
plot(svm_model, trainset)


# RADIAL BASIS FUNCTION KERNALS FOR PRACTICALITY AND FLEXIBLE
# Create complex dataset!
n <- 600
set.seed(42)

df <- data.frame(x1 = rnorm(n, mean = -0.5, sd = 1),
                 x2 = runif(n, min = -1, max = 1))

#set radius and centers
radius <- 0.7
radius_squared <- radius^2
center_1 <- c(-0.7,0)
center_2 <- c(0.7,0)

#classify points
df$y <- 
  factor(ifelse(
    (df$x1-center_1[1])^2 + (df$x2-center_1[2])^2 < radius_squared|
      (df$x1-center_2[1])^2 + (df$x2-center_2[2])^2 < radius_squared,
    -1,1), levels = c(-1,1))

library(ggplot2)

p <- ggplot(data = df, aes(x = x1, y = x2, color = y)) + 
  geom_point() + 
  guides(color = FALSE) +
  scale_color_manual(values = c("red","blue"))

p

#function to generate points on a circle
circle <- function(x1_center, x2_center, r, npoint = 100){
  theta <- seq(0,2*pi, length.out = npoint)
  x1_circ <- x1_center + r * cos(theta)
  x2_circ <- x2_center + r * sin(theta)
  return(data.frame(x1c = x1_circ, x2c = x2_circ))
}
# generate boundary and plot it
boundary_1 <- circle(x1_center = center_1[1],
                     x2_center = center_1[2],
                     r = radius)
p <- p + 
  geom_path(data = boundary_1,
            aes(x = x1c, y = x2c),
            inherit.aes = FALSE)
boundary_2 <- circle(x1_center = center_2[1],
                     x2_center = center_2[2],
                     r = radius)
p <- p + 
  geom_path(data = boundary_2,
            aes(x = x1c, y = x2c),
            inherit.aes = FALSE)

# Split dataset
split <- round(nrow(df) * 0.80)
trainset <- df[1:split,]
testset <- df[(split + 1):nrow(df),]

#Build tuned model
svm_model <- svm( y ~ ., data = trainset, type = "C-classification", 
                 kernel = "polynomial", degree = 2)
svm_model

#Calculate training and test accuracies   

pred_test <- predict(svm_model, testset)
mean(pred_test == testset$y)
#plot model
plot(svm_model, trainset)

# NOW LETS USE THE RADIAL KERNAL!!!!!!!!!!!!!!!!!!!!!!!
svm_model<- svm(y ~ ., 
                data = trainset, 
                type = "C-classification",
                kernel = "radial")

pred_train <- predict(svm_model, trainset)
mean(pred_train==trainset$y) # MUCH BETTER THAN POLYNOMIAL OR LINEAR

pred_test <- predict(svm_model, testset)
mean(pred_test==testset$y) # MUCH BETTER THAN POLYNOMIAL OR LINEAR

#plot decision boundary
plot(svm_model, trainset)

# TUNING PARAMETERS FOR IMPROVEMENT!!!!!!!!!!!!!!!!!!!
#tune parameters
tune_out <- tune.svm(x = trainset[,-3],
                     y = trainset[,3],
                     gamma = 5*10^(-2:2),
                     cost = c(0.01,0.1,1,10,100), 
                     type = "C-classification",
                     kernel = "radial")

#print best values of cost and gamma
tune_out$best.parameters$cost
tune_out$best.parameters$gamma

# USE TUNING PARAMETERS NOW FOR MODEL
svm_model <- svm(y~ .,
                 data=trainset, 
                 type="C-classification", 
                 kernel="radial",
                 cost=tune_out$best.parameters$cost,
                 gamma=tune_out$best.parameters$gamma)
 
mean(pred_test==testset$y) # SLIGHTNLY better
plot(svm_model, trainset) # plot shows that it is clearnly better


# pulling it all together!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
#create vector to store accuracies and set random number seed
accuracy <- rep(NA, 100)
set.seed(2)

#calculate accuracies for 100 training/test partitions
for (i in 1:100){
  df[, "train"] <- ifelse(runif(nrow(df))<0.8, 1, 0)
  trainset <- df[df$train == 1, ]
  testset <- df[df$train == 0, ]
  trainColNum <- grep("train", names(trainset))
  trainset <- trainset[, -trainColNum]
  testset <- testset[, -trainColNum]
  svm_model<- svm(y ~ ., data = trainset, type = "C-classification", kernel = "polynomial", degree = 2)
  pred_test <- predict(svm_model, testset)
  accuracy[i] <- mean(pred_test == testset$y)
}
#print average accuracy and standard deviation
mean(accuracy)
sd(accuracy)

# SAME THING FOR RADIAL BASIS FUNCTION!
#create vector to store accuracies and set random number seed
accuracy <- rep(NA, 100)
set.seed(2)

#calculate accuracies for 100 training/test partitions
for (i in 1:100){
  df[, "train"] <- ifelse(runif(nrow(df))<0.8, 1, 0)
  trainset <- df[df$train == 1, ]
  testset <- df[df$train == 0, ]
  trainColNum <- grep("train", names(trainset))
  trainset <- trainset[, -trainColNum]
  testset <- testset[, -trainColNum]
  svm_model<- svm(y ~ ., data = trainset, type = "C-classification", kernel = "radial")
  pred_test <- predict(svm_model, testset)
  accuracy[i] <- mean(pred_test == testset$y)
}

#print average accuracy and standard deviation
mean(accuracy)
sd(accuracy)

#tune model
tune_out <- tune.svm(x = trainset[, -3], y = trainset[, 3], 
                     gamma = 5*10^(-2:2), 
                     cost = c(0.01, 0.1, 1, 10, 100), 
                     type = "C-classification", kernel = "radial")

#build tuned model
svm_model <- svm(y~ ., data = trainset, type = "C-classification", kernel = "radial", 
                 cost = tune_out$best.parameters$cost, 
                 gamma = tune_out$best.parameters$gamma)

#calculate test accuracy
pred_test <- predict(svm_model, testset)
mean(pred_test == testset$y)

#Plot decision boundary against test data
plot(svm_model, testset)
