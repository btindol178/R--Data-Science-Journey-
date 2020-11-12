require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)
require(lme4)
require(ordinal)
require(MCMCglmm)
# MODEL EVALUATION 
library(pscl)
install.packages("pscl")
library(lubridate)
#
merge32f <- read.csv("merge32F.csv") # this is removed na values of review score and only columns with coments 
merge33f <- read.csv("merge33f.csv") # this is only non na values of review score with status as delivered!!
#This are the two final dataframes now
merge33f <- merge32f
merge34f <- merge33f #added a few more columns 
merge35f <- merge34f # this is the final dummy dataframe 
write.csv(merge35f,file="merge35f.csv")

# MERGE 35 IS ONLY NUMERIC COLUMNS FOR A STEPWISE REGRESSION ANALYSIS OF NUMERICAL COLUMNS 
merge35 <- merge34[,c(7,8,17,20,24,25,26,25,28,29,30,32,37,38,40,41,43,44,47,50,51,52,53,54)]
merge36 <- merge35[-c(14,15)]# remove the two columns with lots of na values
merge37 <- na.omit(merge36) # remove the remaining 27 na values (main stepwise model )

# BEST MODEL GIVEN BY STEPWISE REGRESSION #no product category etc. just numeric
bestmdl <- lm(review_score ~  price + customer_zip_code_prefix 
              + total_payment + delay + order_lead_time , data = merge37)
# LOWEST AIC 
lowestAIC <- lm(review_score ~  price + freight_value + customer_zip_code_prefix 
           + seller_zip_code_prefix + product_name_lenght + product_description_lenght
           + product_photos_qty  + product_length_cm + product_height_cm + product_width_cm
           + total_payment + payment_installments + delay 
           + rev_delay + package_density + order_lead_time , data = merge37)

# LOWEST AIC MODEL BY HAND


# New columns FOR ANALYSIS 
merge32$comment <- 1;
for(i in 1:nrow(merge32))
{
  if(nchar(merge32$review_comment)==0)
  {
    merge32$comment[i] <- 0;
  }
  print(i); 
}

# conversion of columns into correct time stamps
merge33f$review_answer_timestamp<-as.POSIXct(as.character(as.factor(merge33f$review_answer_timestamp)),format="%m/%d/%Y %H:%M")
merge33f$review_creation_date<-as.POSIXct(as.character(as.factor(merge33f$review_creation_date)),format="%m/%d/%Y %H:%M")

merge32$delay <- merge32$order_delivered_customer_date - merge32$order_estimated_delivery_date; # if package is late or not
merge33f$rev_delay <- merge33f$review_answer_timestamp - merge33f$review_creation_date;  #delay of review
merge33f$review_final <- ifelse(merge33f$review_score <=3, 0, 1) # Making a new column if review score less than 3 then 0 if more than 3 then 1
merge33f$ontime_late <- ifelse(merge33f$delay >=0, 1, 0) # MAKING COLUM IF DELIVERY IS LATE
merge33f$package_volume <- merge33f$product_height_cm * merge33f$product_length_cm * merge33f$product_width_cm # package volume 
# PRICE DENSITY AND COST DENSITY DOESNT SEEM TO WORK IN MODELS?
merge33$package_density <- merge34$product_weight_g/merge34$package_volume #density of the package
merge33$price_density <-  merge34$price/merge34$package_density # price per density unit of package
merge33f$cost_density <- merge33f$price/merge33f$product_weight_g
merge33$order_cycle_time <- #how long it takes for a person to re order product is this affected by previous bad review and if so at what level 

# To make certain KPI need to convert columns into TIME FORMAT
merge33f$order_purchase_timestamp<-as.POSIXct(as.character(as.factor(merge33f$order_purchase_timestamp)),format="%m/%d/%Y %H:%M")

merge33f$order_estimated_delivery_date<-as.POSIXct(as.character(as.factor(merge33f$order_estimated_delivery_date)),format="%m/%d/%Y")

merge33f$order_delivered_customer_date<-as.POSIXct(as.character(as.factor(merge33f$order_delivered_customer_date)),format="%m/%d/%Y %H:%M")

#TIME IT TAKES TO PUT IN ORDER TO WHEN IT SHOWS UP
merge33f$order_lead_time <- merge33f$order_delivered_customer_date - merge33f$order_purchase_timestamp

# NEW DATAFRAME SUBSET OF COLUMNS of top 10 PRODUCT CATEGORIES
# MAKING A VECTOR OF THE TOP PRODUCT CATEGORIES
temp <- c("automotivo","ferramentas_jardim",
          "telefonia","relogios_presentes","utilidades_domesticas","informatica_acessorios",
          "moveis_decoracao","esporte_lazer","beleza_saude","cama_mesa_banho")

# MAKING A SUBSET OF THE DATA THAT IS THE TOP PRODUCT CATEGORIES
merge34 <- merge33[merge33$product_category_name %in% temp,]
----------------------------------------------------------------------------------------
# MODELS 
# linear regression simple
mdl_1<- lm(rev_delay ~ delay + total_payment +freight_value + payment_sequential + payment_installments + product_description_lenght + product_photos_qty + as.factor(product_category_name), data = merge34)
#Poisson Regression 
mdl_2<- glm(log(rev_delay) ~ delay + total_payment +freight_value + payment_sequential + payment_installments + product_description_lenght + product_photos_qty + as.factor(product_category_name), data = merge34, family = poisson)

# ORDINAL LOGISTIC REGRESSION
# MAKE DUMMY DATAFRAME THAT HAS REVIEW SCORE AS A FACTOR INSTEAD OF INTIGER FOR ORDERED LOGISTIC MODEL 
merge35 <- merge34 # MERGE34 IS THE MAIN DATAFRAME NOW!!!!!!!!!
merge35$review_score <- as.factor(merge35$review_score)
# GET RID OF MISSING VALUES IN THE MODEL 
merge36 <- merge35[!is.na(merge35$delay), ] 
## fit ordered logit model and store results 'ordmodel.1' WHEN ADD DELAY TO MODEL IT DOESNT WORK!
mdl_3 <- polr(review_score ~ price  , data = merge35, Hess=TRUE) # THIS MODEL WORKS Takes long time


# FIXED EFFECTS LOGISTIC MODELS https://bbolker.github.io/mixedmodels-misc/ecostats_chap.html
#https://cran.r-project.org/web/packages/MCMCglmm/index.html
#https://www.google.com/search?ei=2A3oXZPuI6Lj9AOA44zYBQ&q=ordinal+fixed+effects+in+r&oq=ordinal+fixed+effects+in+r&gs_l=psy-ab.3..33i160j33i299.1488.5461..5580...0.1..1.194.2974.1j24......0....1..gws-wiz.......0i71j0i273j0i131j0j0i67j0i131i273j0i22i30.6Dm2iXzKCwA&ved=0ahUKEwiTupjq4ZzmAhWiMX0KHYAxA1sQ4dUDCAs&uact=5
mdl_4 <- glmer(review_score ~ price + delay + (1|product_category_name), data = merge36, family = "binomial")

cmod_MG1 <- MCMCglmm(review_score~ delay,
                     random=~product_category_name,data=merge36,
                     family="categorical",verbose=FALSE,
                     nitt=5e5,burnin=5e4,thin=100)
----------------------------------------------------------------------
# STEPWISE REGRESSION SELECTING NUMERIC DATA AND REMOVING NA VALUES
merge36 <- merge35[-c(14,15)]# remove the two columns with lots of na values
merge37 <- na.omit(merge36) # remove the remaining 27 na values

# USING STEPWISE REGRESSION TO PICK THE BEST MODEL
library(MASS)
library(tidyverse)
library(caret)
library(leaps)
# FIND BEST AIC WITH STEPWISE REGRESSION (NOT INCLUDING THE PRODUCT CATEGORIES)
# This is a model with all variables minus(product category and price density price density created problems(this also contains no NA values))
mdl1<- lm(review_score ~  price + freight_value + customer_zip_code_prefix 
          + seller_zip_code_prefix + product_name_lenght + product_description_lenght
          + product_photos_qty  + product_length_cm + product_height_cm + product_width_cm
          + total_payment + payment_sequential + payment_installments + delay 
          + rev_delay + package_density + package_volume + order_lead_time , data = merge37)

# THIS MAKES A MODEL FOR STEPWISE REGRESSION
step.model1 <- regsubsets(review_score ~  price + freight_value + customer_zip_code_prefix 
                                   + seller_zip_code_prefix + product_name_lenght + product_description_lenght
                                   + product_photos_qty  + product_length_cm + product_height_cm + product_width_cm
                                   + total_payment + payment_sequential + payment_installments + delay 
                                   + rev_delay + package_density + package_volume + order_lead_time , data = merge37, nvmax = 5,
                                   method = "seqrep")
summary(step.model1) # SUMMARY FOR STEPWISE REGRESSION 
step.model1$results

# Set seed for reproducibility
set.seed(123)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the model
step.model2 <- train(review_score ~  price + freight_value + customer_zip_code_prefix 
                     + seller_zip_code_prefix + product_name_lenght + product_description_lenght
                     + product_photos_qty  + product_length_cm + product_height_cm + product_width_cm
                     + total_payment + payment_sequential + payment_installments + delay 
                     + rev_delay + package_density + package_volume + order_lead_time , data = merge37,
                    method = "leapBackward", 
                    tuneGrid = data.frame(nvmax = 1:5),
                    trControl = train.control)


# top 5 models for best RMSE etc. 
step.model2$results                    

# The number of variables in the best model in this case 5!
step.model2$bestTune

# BEST MODEL VARIABLES WITH * ARE BEST 5 because optimal number is 5
summary(step.model2$finalModel)

library(MASS)
library(DescTools)
# BEST MODEL USING STEPWISE REGRESSION WITH PACKAGE CARET
bestmdl <- polr(freview_score ~  price + delay + order_lead_time , data = merge37)
PseudoR2(bestmdl)
-------------------------------------------------------------------------
  # FINDING BEST MODEL ACCORDING TO AIC 
s
AIC(mdl1) # 232665.8

# minus package volume
mdl2 <- lm(review_score ~  price + freight_value + customer_zip_code_prefix 
                  + seller_zip_code_prefix + product_name_lenght + product_description_lenght
                  + product_photos_qty  + product_length_cm + product_height_cm + product_width_cm
                  + total_payment + payment_sequential + payment_installments + delay 
                  + rev_delay + package_density + order_lead_time , data = merge37)
AIC(mdl2) # 232663.8

#minus product_potos_qty

AIC(mdl3) #232662.9

# BEST AIC (LOWEST)
# minus payment sequential (THE AIC DOESNT GET ANY SMALLER AFTER THIS MODEL)
mdl4 <- lm(review_score ~  price + freight_value + customer_zip_code_prefix 
           + seller_zip_code_prefix + product_name_lenght + product_description_lenght
           + product_photos_qty  + product_length_cm + product_height_cm + product_width_cm
           + total_payment + payment_installments + delay 
           + rev_delay + package_density + order_lead_time , data = merge37)
AIC(mdl4) #232662.6

# TO DO NEXT 
#WHY ARE ALL BOXPLOTS SHOWING SO MANY OUTLIERS (FIX TO MAKE R squared lower)
# discrete choice models
#cart algorithms and mars for R (REGRESSION TREES AND MULTIVARIATE REGRESSION SPLINES)
# multinomial model
#if same customer bought same product id from 2 different customers
#regression needs to have no dependence between independent variables what a person buys depends on what he just bought

