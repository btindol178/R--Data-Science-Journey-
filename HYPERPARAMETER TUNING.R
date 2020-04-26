# HYPER PARAMETER TUNING !!!!!!!!!!!!!!
bcdf <- read.csv("breastcancerdata.csv")

# Fit a linear model on the breast_cancer_data.
linear_model <- lm(concavity_mean ~ symmetry_mean,
                   data = bcdf)

# Look at the summary of the linear_model.
summary(linear_model)

# Extract the coefficients.
linear_model$coefficients

library(ggplot2)

# Plot linear relationship.
#To get a good feel for the difference between fitted model parameters and hyperparameters, we are going to take a closer look at those fitted parameters: 
# in our simple linear model, the coefficients. The dataset breast_cancer_data has already been loaded for you and the linear model call was run as in the previous lesson, so you can directly access the object linear_model.
ggplot(data = bcdf, 
       aes(x = symmetry_mean, y = concavity_mean)) +
  geom_point(color = "grey") +
  geom_abline(slope = linear_model$coefficients[2], 
              intercept = linear_model$coefficients[1])

#CARET MACHINE LEARNING!!!!!!!
# Load caret and set seed
library(caret)
set.seed(42)

# Create partition index
# splitting data!
index <- createDataPartition(bcdf$ï..diagnosis, p = .70,list = FALSE)

# Subset `breast_cancer_data` with index
bc_train_data <- bcdf[index, ]
bc_test_data  <- bcdf[-index, ]

library(caret)
install.packages("tictoc")
library(tictoc)

# Repeated CV.
# setting up cross validation
fitControl <- trainControl(method = "repeatedcv",
                           number = 3,
                           repeats = 5)

# TRAIN A RANDOM FOREST MODEL
tic()
set.seed(42)
rf_model <- train(ï..diagnosis ~ ., 
                  data = bc_train_data, 
                  method = "rf",  # random forest
                  trControl = fitControl,
                  verbose = FALSE)

toc()
rf_model


# Set seed.
set.seed(42)
# Start timer.
tic()
# Train model.
gbm_model <- train(ï..diagnosis ~ ., 
                   data = bc_train_data, 
                   method = "gbm", 
                   trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3),
                   verbose = FALSE,
                   tuneLength = 4)
# Stop timer.
toc()
gbm_model

# Define hyperparameter grid.
hyperparams <- expand.grid(n.trees = 200, 
                           interaction.depth = 1, 
                           shrinkage = 0.1, 
                           n.minobsinnode = 10)

set.seed(42)
# Apply hyperparameter grid to train().
gbm_model <- train(ï..diagnosis ~ ., 
                   data = bc_train_data, 
                   method = "gbm", 
                   trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3),
                   verbose = FALSE,
                   tuneGrid = hyperparams)
gbm_model

# Lets try hyper parameter tuning again new dataset
# DONT FORGET ABOUT UNBALANCED CLASSES IN REAL LIFE 

library(tidyverse)
voters_data <- read.csv("votersdata.csv")
colnames(voters_data)[1]<- "turnout16_2016"
smp_size <- floor(0.75 * nrow(voters_data))
set.seed(123)
train_ind <- sample(seq_len(nrow(voters_data)), size = smp_size)
voters_train_data<- voters_data[train_ind, ]
voters_test_data<- voters_data[-train_ind, ]


library(caret)
library(tictoc)

fitControl <- trainControl(method = "repeatedcv",
                           number = 3,
                           repeats = 5)
tic()
set.seed(42)
gbm_model_voters <- train(turnout16_2016 ~ ., 
                          data = voters_train_data, 
                          method = "gbm", 
                          trControl = fitControl,
                          verbose = FALSE)
toc()
gmb_model_voters

# CARTESIAN GRID OF HYPER PARAMETERS
# EVERY COMBINATION OF HYPERPARAMETERS WILL BE USED
man_grid <-  expand.grid(n.trees = c(100, 200, 250),
                         interaction.depth = c(1, 4, 6), 
                         shrinkage = 0.1,
                         n.minobsinnode = 10)

fitControl <- trainControl(method = "repeatedcv",
                           number = 3,
                           repeats = 5)
tic()
set.seed(42)
gbm_model_voters_grid <- train(turnout16_2016 ~ ., 
                               data = voters_train_data, 
                               method = "gbm", 
                               trControl = fitControl,
                               verbose = FALSE,
                               tuneGrid = man_grid)
toc()
gbm_model_voters_grid

plot(gbm_model_voters_grid)

plot(gbm_model_voters_grid, 
     metric = "Kappa", 
     plotType = "level")
# KAPPA OBSERVED ACCURACY VS EXPECTED ACCURACY
# KAPPA VALUES NOT GOOD BECAUSE OF CLASS IMBALANCE
# ALTHOUGH HIGH ACCURACY MODEL DIDNT PERFORM MUCH BETTER THAN RANDOM

# GRID VS. RANDOM SEARCH
# LETS DEFINE A RANGE OF VALUES NOW IN GRID
big_grid <-  expand.grid(n.trees = seq(from = 10, to = 300, by = 50),
                         interaction.depth = seq(from = 1, to = 10, 
                                                 length.out = 6), 
                         shrinkage = 0.1,
                         n.minobsinnode = 10)

fitControl <- trainControl(method = "repeatedcv",
                           number = 3,
                           repeats = 5,
                           search = "grid")
tic()
set.seed(42)
gbm_model_voters_big_grid <- train(turnout16_2016 ~ ., 
                                   data = voters_train_data, 
                                   method = "gbm", 
                                   trControl = fitControl,
                                   verbose = FALSE,
                                   tuneGrid = big_grid)
toc()
gbm_model_voters_big_grid

library(ggplot2)
ggplot(gbm_model_voters_big_grid)
#carteasion grid search is computationally expensive
# random search is a way to get around this

# LETS PERFORM RANDOM SEARCH NOW!!!!!!!!!!!! NOT CARTEASION
fitControl <- trainControl(method = "repeatedcv",
                           number = 3,
                           repeats = 5,
                           search = "random")

set.seed(42)
gbm_model_voters_random <- train(turnout16_2016 ~ ., 
                                 data = voters_train_data, 
                                 method = "gbm", 
                                 trControl = fitControl,
                                 verbose = FALSE,
                                 tuneLength = 5) # 5 TO SMALL WANT IN REAL WORLD TO DO AT LEAST 100
gbm_model_voters_random
# RANDOM CANNOT BE COMBINED WITH GRID SEARCH

# LETS TRAIN A NEURAL NETWORK!
# Define the grid with hyperparameter ranges
big_grid <- expand.grid(size = seq(from = 1, to = 5, by = 1), decay = c(0, 1))

# Train control with grid search
fitControl <- trainControl(method = "repeatedcv", number = 3, repeats = 5, search = "grid")

# Train neural net
tic()
set.seed(42)
nn_model_voters_big_grid <- train(turnout16_2016 ~ ., 
                                  data = voters_train_data, 
                                  method = "nnet", 
                                  trControl = fitControl,
                                  verbose = FALSE,
                                  tuneGrid = big_grid)
nn_model_voters_big_grid
# remember not great due to class imbalance

# LETS DO RANDOM SEARCH NOW!!!!!!! NOT GRID
# Train control with random search
fitControl <- trainControl(method = "repeatedcv",
                           number = 3,
                           repeats = 5,
                           search = "random")

nn_model_voters_big_grid <- train(turnout16_2016 ~ ., 
                                  data = voters_train_data, 
                                  method = "nnet", 
                                  trControl = fitControl,
                                  verbose = FALSE,
                                  tuneLength = 6)

# ADAPTIVE RESAMPLING TIME @!!!!!!!!!!!!!!!!!!!!!!!!!!
#Grid Search
#All hyperparameter combinations are computed.

#Random Search
#Random subsets of hyperparameter combinations are computed.
#Evaluation of best combination is done at the end.

#Adaptive Resampling
#Hyperparameter combinations are resampled with values near combinations that performed well.
#SUB OPTIMAL VALUES NOT TESTED AT ALL
#Adaptive Resampling is, therefore, faster and more efficient!
#trainControl: method = "adaptive_cv" + search = "random" + adaptive =
#min: minimum number of resamples per hyperparameter
#alpha: confidence level for removing hyperparameters
#method: "gls" for linear model or "BT" for Bradley-Terry
#complete: if TRUE generates full resampling set
library(plyr); library(dplyr)
library(caret)
# Define trainControl function
fitControl <- trainControl(method = "adaptive_cv",
                           number = 3, repeats = 3,
                           adaptive = list(min = 3, alpha = 0.05, method = "BT", complete = FALSE),
                           search = "random")

# Start timer & train model
tic()
svm_model_voters_ar <- train(turnout16_2016 ~ ., 
                             data = voters_train_data, 
                             method = "nnet", 
                             trControl = fitControl,
                             verbose = FALSE,
                             tuneLength = 6) # at least 100 in real world

# MACHINE LEARNING WITH MLR!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#Model training follows three steps:
# Define the task
# Define the learner
# Fit the model
knowledge_data <- read.csv("knowledgedata.csv")
colnames(knowledge_data)[1]<- "STG"
 
# SUPERVISED LEARNING 
#ClassifTask() for binary and multi-class classification
#MultilabelTask() for multi-label classification problems
#CostSensTask() for general cost-sensitive classification
library(caret)
set.seed(42)

# Create partition index
# splitting data!
index <- createDataPartition(knowledge_data$UNS, p = .70,list = FALSE)

# Subset `breast_cancer_data` with index
knowledge_train_data <- knowledge_data[index, ]
knowledge_test_data  <- knowledge_data[-index, ]

install.packages("mlr")
library(mlr)
install.packages("h2o")
library(h2o)
task <- makeClassifTask(data = knowledge_train_data, 
                        target = "UNS")

# CAN CHOOSE ALGORITHM FROM listLearners()
listLearners()
# Call the list of learners
listLearners() %>%
  as.data.frame() %>%
  select(class, short.name, package) %>%
  filter(grepl("classif.", class))
# Define task
task <- makeClassifTask(data = knowledge_train_data, 
                        target = "UNS")
# Define learner
lrn <- makeLearner("classif.h2o.deeplearning", 
                   fix.factors.prediction = TRUE, # IF HAVE MORE/LESS UNIQUE FACTORS IN PREDICTOR THAN IN INDEPENDENT THAN SET THIS TO TRUE
                   predict.type = "prob")
tic()

# Fit model
model <- train(lrn,task)
# i need to get the 64 bit version of java for H20 TO WORK!!!!!!!

# TRY ANOTHER ALGORITHM!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Create classification taks
task <- makeClassifTask(data = knowledge_train_data, 
                        target = "UNS")

# Create learner
lrn <- makeLearner("classif.randomForest", 
                   predict.type = "prob", 
                   fix.factors.prediction = TRUE)
model <- train(lrn,task)
model

# HYPERPARAMETER TUNING IN MLR!!!!!!!!!!!!!!!
#the search space for every hyperparameter
#the tuning method (e.g. grid or random search)
#the resampling method
# DEFINE SEARCH SPACE BELOW
makeParamSet(
  makeNumericParam(),
  makeIntegerParam(),
  makeDiscreteParam(),
  makeLogicalParam(),
  makeDiscreteVectorParam()
)
getParamSet("classif.h2o.deeplearning") # FOR DEEPLEARNING MODEL

param_set <- makeParamSet( # HYPER PARAMETERS FOR DEEP LEARNING
  makeDiscreteParam("hidden", values = list(one = 10, two = c(10, 5, 10))),
  makeDiscreteParam("activation", values = c("Rectifier", "Tanh")),
  makeNumericParam("l1", lower = 0.0001, upper = 1),
  makeNumericParam("l2", lower = 0.0001, upper = 1)
)
#GRID SEARCH Can only deal with discrete parameter sets!
ctrl_grid <- makeTuneControlGrid()
ctrl_grid

#RANDOM SEARCH ITERATION DEFAULT 100 Can deal with all types of parameter sets
ctrl_random <- makeTuneControlRandom()
ctrl_random

# Resampling strategy!
cross_val <- makeResampleDesc("RepCV", 
                              predict = "both",folds = 5 * 3)

param_set <- makeParamSet( # HYPER PARAMETERS FOR DEEP LEARNING
  makeDiscreteParam("hidden", values = list(one = 10, two = c(10, 5, 10))),
  makeDiscreteParam("activation", values = c("Rectifier", "Tanh")),
  makeNumericParam("l1", lower = 0.0001, upper = 1),
  makeNumericParam("l2", lower = 0.0001, upper = 1)
)
ctrl_grid <- makeTuneControlGrid()

task <- makeClassifTask(data = knowledge_train_data, 
                        target = "UNS")

lrn <- makeLearner("classif.h2o.deeplearning", 
                   predict.type = "prob", 
                   fix.factors.prediction = TRUE)

lrn_tune <- tuneParams(lrn,
                       task,
                       resampling = cross_val,
                       control = ctrl_grid,
                       par.set = param_set)

# LETS TRY ANOTHER MODEL!!!!!!!!!!!!!!!
# Get the parameter set for neural networks of the nnet package
getParamSet("classif.nnet")

# Define task
task <- makeClassifTask(data = knowledge_train_data, 
                        target = "UNS")

# Define learner
lrn <- makeLearner("classif.nnet", predict.type = "prob", fix.factors.prediction = TRUE)

# Define set of parameters
param_set <- makeParamSet(
  makeDiscreteParam("size", values = c(2,3,5)),
  makeNumericParam("decay", lower = 0.0001, upper = 0.1)
)
#Define a random search tuning method.
ctrl_random <- makeTuneControlRandom(maxit = 6)

# Define a 3 x 3 repeated cross-validation scheme
cross_val <- makeResampleDesc("RepCV", folds = 3 * 3)

# Tune hyperparameters
tic()
lrn_tune <- tuneParams(lrn,
                       task,
                       resampling = cross_val,
                       control = ctrl_random,
                       par.set = param_set)
toc()

# EVALUATING TUNED HYPERPARAMETERS
#How different hyperparameters affect the performance of our model.
#Which hyperparameters have a particularly strong or weak impact on our model performance.
#Whether our hyperparameter search converged, i.e. whether we can be reasonably confident that we found the most optimal hyperparameter combination (or close to it).

getParamSet("classif.h2o.deeplearning")
param_set <- makeParamSet(
  makeDiscreteParam("hidden", values = list(one = 10, two = c(10, 5, 10))),
  makeDiscreteParam("activation", values = c("Rectifier", "Tanh")),
  makeNumericParam("l1", lower = 0.0001, upper = 1),
  makeNumericParam("l2", lower = 0.0001, upper = 1)
)
ctrl_random <- makeTuneControlRandom(maxit = 50)
holdout <- makeResampleDesc("Holdout")
task <- makeClassifTask(data = knowledge_train_data, target = "UNS")

lrn <- makeLearner("classif.h2o.deeplearning", predict.type = "prob", 
                   fix.factors.prediction = TRUE)

lrn_tune <- tuneParams(lrn,
                       task,
                       resampling = holdout,
                       control = ctrl_random,
                       par.set = param_set)
lrn_tune

# which hyper parameters were used and worked and such
generateHyperParsEffectData(lrn_tune, partial.dep = TRUE) 

hyperpar_effects <- generateHyperParsEffectData(lrn_tune, partial.dep = TRUE)
plotHyperParsEffect(hyperpar_effects, partial.dep.learn = "regr.randomForest",
                    x = "l1", y = "mmce.test.mean", z = "hidden",
                    plot.type = "line")

# TRY IT AGAIN 
# EVALUATION OF HYPER PARAMETER TUNING PARAMETERS!!!!!
task <- makeClassifTask(data = knowledge_train_data, 
                        target = "UNS")

lrn <- makeLearner(cl = "classif.rpart", fix.factors.prediction = TRUE)

param_set <- makeParamSet(
  makeIntegerParam("minsplit", lower = 1, upper = 30),
  makeIntegerParam("minbucket", lower = 1, upper = 30),
  makeIntegerParam("maxdepth", lower = 3, upper = 10)
)

ctrl_random <- makeTuneControlRandom(maxit = 10)

install.packages("mmpf")
library(mmpf)
# Create holdout sampling
holdout <- makeResampleDesc("Holdout")

# Perform tuning
lrn_tune <- tuneParams(learner = lrn, task = task, resampling = holdout, control = ctrl_random, par.set = param_set)

# Generate hyperparameter effect data
hyperpar_effects <- generateHyperParsEffectData(lrn_tune, partial.dep = TRUE)

# Plot hyperparameter effects
plotHyperParsEffect(hyperpar_effects, 
                    partial.dep.learn = "regr.glm",
                    x = "minsplit", y = "mmce.test.mean", z = "maxdepth",
                    plot.type = "line")

# ADVANCED TUNING WITH MLR!!!!!!!!!!!!!!!!!!!!!
#makeTuneControlCMAES: CMA Evolution Strategy
#makeTuneControlDesign: Predefined data frame of hyperparameters
#makeTuneControlGenSA: Generalized simulated annealing
#makeTuneControlIrace: Tuning with iterated F-Racing
#makeTuneControlMBO: Model-based / Bayesian optimization
install.packages("GenSA")
library(GenSA)
# Generalized simulated annealing
ctrl_gensa <- makeTuneControlGenSA()

# Create holdout sampling
bootstrap <- makeResampleDesc("Bootstrap", predict = "both")

# Perform tuning
lrn_tune <- tuneParams(learner = lrn, 
                       task = task, 
                       resampling = bootstrap, 
                       control = ctrl_gensa, 
                       par.set = param_set,
                       measures = list(acc, 
                                       setAggregation(acc, train.mean), 
                                       mmce, 
                                       setAggregation(mmce, train.mean)))

# nested cross validation and nested resampling
lrn_wrapper <- makeTuneWrapper(learner = lrn, 
                               resampling = bootstrap, 
                               control = ctrl_gensa, 
                               par.set = param_set,
                               measures = list(acc, mmce))

model_nested <-  train(lrn_wrapper, task)
getTuneResult(model_nested)

# or choose hypter parameter from a tuning set
lrn_best <- setHyperPars(lrn, par.vals = list(minsplit = 4, 
                                              minbucket = 3, 
                                              maxdepth = 6))
model_best <- train(lrn_best, task)

# LETS TUNE PARAMETERS WITH ADVANCED METHODS NOW!!!!!!!!!!!!!!!!!!
task <- makeClassifTask(data = knowledge_train_data, 
                        target = "UNS")

lrn <- makeLearner(cl = "classif.nnet", fix.factors.prediction = TRUE)

param_set <- makeParamSet(
  makeIntegerParam("size", lower = 1, upper = 5),
  makeIntegerParam("maxit", lower = 1, upper = 300),
  makeNumericParam("decay", lower = 0.0001, upper = 1)
)

ctrl_random <- makeTuneControlRandom(maxit = 10)

# Create holdout sampling
holdout <- makeResampleDesc("Holdout", predict = "both")

# Perform tuning
lrn_tune <- tuneParams(learner = lrn, 
                       task = task, 
                       resampling = holdout, 
                       control = ctrl_random, 
                       par.set = param_set,
                       measures = list(mmce, setAggregation(mmce, train.mean), acc, setAggregation(acc, train.mean)))

library(tidyverse)

task <- makeClassifTask(data = knowledge_train_data, 
                        target = "UNS")

lrn <- makeLearner(cl = "classif.nnet", fix.factors.prediction = TRUE)

# Set hyperparameters
#Set the following hyperparameters for a neural net: One hidden layer, maximum number of iterations of 150 and 0 decay.
lrn_best <- setHyperPars(lrn, par.vals = list(size = 1, 
                                              maxit = 150, 
                                              decay = 0))

# Train model
model_best <- train(lrn_best, task)

##########################################################################################################################################################
# MACHINE LEARNING WITH H20!!!!!!!!!!!!!!!!!!!!!!!!

library(h2o)
# DOWNLOAD 64BIT JAVA AND FIND ITS FILE PATH AND SET ENVIORNMENT TO THAT BELOW
Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk1.8.0_231") ##your own path of Java SE intalled
h2o.init()

seeds_data <- read.csv("seedsdatast.csv")
colnames(seeds_data)[1]<- "area"

# MAKE AS H20 DATAFRAME
seeds_data_hf <- as.h2o(seeds_data)

# Define features and target variable
y <- "seed_type"
x <- setdiff(colnames(seeds_data_hf), y)

# MAKE TRAINING VALIDATION AND TEST SETS
sframe <- h2o.splitFrame(data = seeds_data_hf, 
                         ratios = c(0.7, 0.15), #70% in train 15 in validation and other 15% in test set
                         seed = 42)

# SETTING TRAIN VALID AND TEST
train <- sframe[[1]]
valid <- sframe[[2]]
test <- sframe[[3]]

# GETTING SPLIT PARAMETERS
summary(train$seed_type, exact_quantiles = TRUE)
summary(test$seed_type, exact_quantiles = TRUE)

# MODEL TRAINING WITH H20 ALGORITHMS
#Gradient Boosted models with h2o.gbm() & h2o.xgboost()
#Generalized linear models with h2o.glm()
#Random Forest models with h2o.randomForest()
#Neural Networks with h2o.deeplearning()
gbm_model <- h2o.gbm(x = x, y = y,
                     training_frame = train, validation_frame = valid)

# Train random forest model
rf_model <- h2o.randomForest(x = x,
                             y = y,
                             training_frame = train,
                             validation_frame = valid)
#Model performance
perf <- h2o.performance(gbm_model, test)
perf
h2o.confusionMatrix(perf)

# Calculate model performance
perf <- h2o.performance(rf_model, valid = TRUE)
perf

# Extract logloss
h2o.logloss(perf)

# grid search with h20 
# Define hyperparameters
# Define hyperparameters
dl_params <- list(hidden = list(c(50, 50), c(100, 100)),
                  epochs = c(5, 10, 15),
                  rate = c(0.001, 0.005, 0.01))
# Define search criteria
search_criteria <- list(strategy = "RandomDiscrete", 
                        max_runtime_secs = 10, # this is way too short & only used to keep runtime short!
                        seed = 42)

# Train with random search
dl_grid <- h2o.grid("deeplearning", 
                    grid_id = "dl_grid",
                    x = x, 
                    y = y,
                    training_frame = train,
                    validation_frame = valid,
                    seed = 42,
                    hyper_params = dl_params,
                    search_criteria = search_criteria)

# Define early stopping
stopping_params <- list(strategy = "RandomDiscrete", 
                        stopping_metric = "misclassification",
                        stopping_rounds = 2, 
                        stopping_tolerance = 0.1,
                        seed = 42)

#AUTOMATIC MACHINE LEARNING AutoML!!!!!!!!!!!!
#Automatic tuning of algorithms, in addition to hyperparameters
#AutoML makes model tuning and optimization much faster and easier
#AutoML only needs a dataset, a target variable and a time or model number limit for training
#Generalized Linear Model (GLM)
#(Distributed) Random Forest (DRF)
#Extremely Randomized Trees (XRT)
#Extreme Gradient Boosting (XGBoost)
#Gradient Boosting Machines (GBM)
#Deep Learning (fully-connected multi-layer artificial neural network)
#Stacked Ensembles (of all models & of best of family)
# AUTOML USES ALL OF ABOVE ALGORITHMS IN ORDER!!!!!!!
#returns a leaderboard of all models, ranked by the chosen metric (here "logloss")
seeds_train_data_hf <- as.h2o(seeds_data)

y <- "seed_type"
x <- setdiff(colnames(seeds_train_data_hf), y)

seeds_train_data_hf[, y] <- as.factor(seeds_train_data_hf[, y])

sframe <- h2o.splitFrame(seeds_train_data_hf, seed = 42)
train <- sframe[[1]]
valid <- sframe[[2]]

# Run automatic machine learning
automl_model <- h2o.automl(x = x, 
                           y = y,
                           training_frame = train,
                           max_runtime_secs = 10,
                           seed = 42)

# Run automatic machine learning
automl_model <- h2o.automl(x = x, 
                           y = y,
                           training_frame = train,
                           max_runtime_secs = 10,
                           sort_metric = "mean_per_class_error",
                           nfolds = 3,
                           seed = 42)

# Extract the leaderboard
lb <- automl_model@leaderboard
head(lb)

# Assign best model new object name
aml_leader <- automl_model@leader

# Look at best model
summary(aml_leader)

# Run automatic machine learning
automl_model <- h2o.automl(x = x, 
                           y = y,
                           training_frame = train,
                           max_runtime_secs = 10,
                           sort_metric = "mean_per_class_error",
                           leaderboard_frame = valid,
                           seed = 42)